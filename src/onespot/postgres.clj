(ns onespot.postgres
  (:require [clojure.string :as s]
            [clojure.core.memoize :as m]
            [clojure.edn :as edn]
            ;;
            [taoensso.timbre :as log]
            ;;
            [next.jdbc            :as jdbc]
            [next.jdbc.sql        :as sql]
            [next.jdbc.prepare    :as p]
            [next.jdbc.result-set :as rs]
            [next.jdbc.date-time  :as jdbc-dt]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [oberon.utils :refer [dump-> dump->>]]
            [onespot.core :as os])
  (:import [java.sql
            Date Timestamp
            Array
            Clob
            PreparedStatement
            ResultSet ResultSetMetaData
            Statement
            SQLException]
           [org.postgresql.util PGobject]
           ;;
           [java.time LocalDate Instant ZoneId]
           [java.time.format DateTimeFormatter]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp (atom nil))
(defn-   ^:private set-tmp!
  [value]
  (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------
;;  Default config for dates, extended by jdbc.next.

(jdbc-dt/read-as-local)

;;; --------------------------------------------------------------------------------
;;  Onespot layer

(defn entity-id
  [entity-id]
  (-> entity-id os/canonical-entity-id os/pull ::entity-id))

(defn get-table
  [entity-id]
  (or (-> (os/rec entity-id) ::info :table)
      (throw (ex-info (format "Failed to get DB Table for %s" entity-id)
                      {:entity-id entity-id}))))

;;; --------------------------------------------------------------------------------

(def ^:dynamic *datasource*)
(def ^:dynamic *connection*)
(def ^:dynamic *transaction*)

(defmacro with-datasource [datasource & body]
  `(let [datasource# ~datasource]
     (binding [*datasource* datasource#]
       ~@body)))

(defmacro with-connection [& body]
  `(with-open [connection# (jdbc/get-connection *datasource* {:read-only true})]
     (binding [*connection* connection#]
       ~@body)))

(defmacro in-transaction [& body]
  `(with-open [connection# (jdbc/get-connection *datasource*)]
     (binding [*connection* connection#]
       (jdbc/with-transaction [tx# *connection*]
         (binding [*transaction* tx#]
           ~@body)))))

;;; --------------------------------------------------------------------------------
;;  Simple value converters
;;
;;  FIXME:: Move all of this into the oberon coerce library.

(def +yyyy-mm-dd+ (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
(def +yyyy-mm+    (DateTimeFormatter/ofPattern "yyyy-MM"))
(def +yyyymmdd+   (DateTimeFormatter/ofPattern "yyyyMMdd"))

(defn ld->yyyy-mm-dd
  [^LocalDate ld]
  (.format ld +yyyy-mm-dd+))

(defn ld->yyyy-mm
  [ld]
  (.format ld +yyyy-mm+))

(defn yyyy-mm-dd->ld
  [s]
  (LocalDate/parse s +yyyy-mm-dd+))

(defn ld->yyyymmdd
  [ld]
  (.format ld +yyyy-mm-dd+))

(defn yyyymmdd->ld
  [s]
  (LocalDate/parse s +yyyymmdd+))

(defn java-date->sql
  [jd]
  (let [ld (.. jd
               toInstant
               (atZone (ZoneId/of "UTC"))
               toLocalDate)]
    (Date/valueOf ld)))

;;; --------------------------------------------------------------------------------
;;  WRITING TO DATABASE, INCLUDING COERCION FOR QUERIES

(defn make-pg-object
  [object-type object-value]
  (doto (PGobject.)
    (.setType  (name object-type))
    (.setValue object-value)))

(defn make-pg-array
  [pg-type coll]
  (.createArrayOf *connection* pg-type (to-array coll)))

(def get-pg-value (memfn getValue))
(def get-pg-type  (memfn getType))

;;; --------------------------------------------------------------------------------
;;  Custom PG type handling

(defn as-db-name
  [k]
  (let [n (-> (name k) (s/replace "?" ""))]
   (csk/->snake_case_keyword n)))

(def make-enum
  (let [f (fn [enum-type enum-value]
            (when enum-value
              (make-pg-object (csk/->SCREAMING_SNAKE_CASE_STRING enum-type) (name enum-value))))]
    (m/fifo f {} :fifo/threshold 1024)))

(defn make-daterange
  [[from to]]
  (make-pg-object :daterange
                  (str "["
                       (some-> from ld->yyyy-mm-dd)
                       ","
                       (some-> to ld->yyyy-mm-dd)
                       "]")))

(defn read-daterange
  [pgobj]
  ;; "[2023-11-06,2023-11-28)"
  ;; "[2023-11-06,)"
  ;; "[,2023-11-28)"
  (let [[from to] (-> pgobj
                      get-pg-value
                      (s/split #","))
        from (some->> from (drop 1) seq (apply str) yyyy-mm-dd->ld)
        to   (some-> (some->> to   butlast  seq (apply str) yyyy-mm-dd->ld)
                     (.plusDays -1))]
    [from to]))

;;; FIXME: Add these ranges in too
;; int4range — Range of integer, int4multirange — corresponding Multirange
;; int8range — Range of bigint, int8multirange — corresponding Multirange
;; numrange — Range of numeric, nummultirange — corresponding Multirange
;; tsrange — Range of timestamp without time zone, tsmultirange — corresponding Multirange
;; tstzrange — Range of timestamp with time zone, tstzmultirange — corresponding Multirange

(defn make-ltree
  [v]
  (make-pg-object :ltree v))

;;;

(defmulti clj->db (fn [v] (type v)))

(defmethod clj->db :default
  [v]
  v)

(defmethod clj->db java.util.Date
  [v]
  (java-date->sql v))

(defmethod clj->db java.time.LocalDate
  [v]
  (Date/valueOf v))

(defmethod clj->db java.time.Instant
  [v]
  (Timestamp/from v))

(defmethod clj->db clojure.lang.Keyword
  [v]
  (name v))

;;;

(defmulti entity->db (fn [{:keys [entity-id] ::keys [info] :as entity} v]
                       (or (:type info) entity-id)))

(defmethod entity->db :default
  [entity v]
  (clj->db v))

(defmethod entity->db ::os/edn-map
  [entity v]
  (some-> v pr-str))

(defmethod entity->db ::os/keyword
  [entity v]
  (name v))

(defmethod entity->db ::enum
  [{::keys [entity-id info] :as entity} v]
  (make-enum (or (:enum-type info)
                 entity-id
                 (os/entity-id entity))
             v))

(defmethod entity->db ::text-array
  [entity v]
  (make-array "TEXT" v))

(defmethod entity->db ::int-array
  [entity v]
  (make-array "INT" v))

(defmethod entity->db ::date-array
  [entity v]
  (make-array "DATE" (map clj->db v)))

(defmethod entity->db ::date-range
  [entity v]
  (make-daterange v))

(defmethod entity->db ::instant-array
  [entity v]
  ;; FIXME:: Should these be TIMESTAMPS?
  (make-array "TIMESTAMPTZ" (map clj->db v)))

;;;

(defn record->row
  [record & {:keys [domain db-names?]}]
  (->> record
       (map (fn [[k v]]
              (cond
                (nil? v) nil
                ;;
                ;; If it's an attr then we use the attr's entity to do
                ;; the conversion.  Attrs are `named` things that
                ;; point to another type of entity so they don't have
                ;; any ::info options themselves.
                (and (os/registered? k)
                     (os/attr?       k))
                (let [entity       (os/attr k)
                      entity-id    (::entity-id entity)
                      os-entity-id (os/entity-id  entity)]
                  [(if db-names?
                     (-> (or entity-id os-entity-id) as-db-name)
                     os-entity-id)
                   ;; If the attr-entity isn't specialised for ::kind
                   ;; then the native coercion will be called.
                   (entity->db (os/attr-entity os-entity-id) v)])
                ;;
                :else [(if db-names?
                         (as-db-name k)
                         k)
                       (clj->db v)])))
       (into {})))

;;; --------------------------------------------------------------------------------

(defmulti db->entity (fn [{:keys [entity-id] ::keys [info] :as entity} v]
                       (or (:type info) entity-id)))

(defmethod db->entity :default
  [entity v]
  v)

(defmethod db->entity ::os/keyword
  [entity v]
  (keyword v))

(defmethod db->entity ::os/edn-map
  [entity v]
  (when v (edn/read-string v)))

(defmethod db->entity ::enum
  [entity v]
  (keyword v))

(defmethod db->entity ::array
  [entity v]
  (some-> v .getArray))

(defmethod db->entity ::keyword-array
  [entity v]
  (some->> v
           .getArray
           (map keyword)))

(defn row->record
  [attr-map row]
  (->> row
       (map (fn [[k v]]
              (let [entity-id (get attr-map k k)]
                (cond
                  (not (os/registered? entity-id)) [k v]
                  ;;
                  (os/attr? entity-id) (let [attr        (os/attr entity-id)
                                             attr-entity (os/attr-entity attr)]
                                          [entity-id (db->entity attr-entity v)])
                  :else [entity-id (db->entity (os/pull entity-id) v)]))))
       (into {})))

(defn make-row->record-attr-map
  "When the entity has a different entity-id in the `postgres` world we
  map it to the onespot entity-id so that we can correctly do the
  translations and coercions later on."
  []
  (->> (os/attrs)
       (map (fn [attr]
              (let [entity-id    (entity-id    attr)
                    os-entity-id (os/entity-id attr)]
                (when-not (= entity-id os-entity-id)
                  [entity-id os-entity-id]))))
       (into {})))

;;; --------------------------------------------------------------------------------
;;; READING FROM DATABASE

(defmulti read-object (fn [object-type object]
                        object-type))

(defmethod read-object :default
  [object-type object]
  (log/warn (format "No read-object method found for: `%s`." object-type))
  object)

(defmethod read-object :ltree
  [_ v]
  (get-pg-value v))

(let [object-reader (fn [object]
                      (read-object (-> object get-pg-type keyword) object))]
  (extend-protocol rs/ReadableColumn
    PGobject
    (read-column-by-label [^PGobject object _]          (object-reader object))
    (read-column-by-index [^PGobject object rsmeta idx] (object-reader object))))

#_
(defn pg-array?
  [x]
  (instance? org.postgresql.jdbc.PgArray x))

;;; --------------------------------------------------------------------------------

(defn get-column-names
  [^ResultSetMetaData rsmeta]
  (mapv (fn [^Integer i]
          (let [type  (-> (.getColumnTypeName rsmeta i) csk/->kebab-case-keyword)
                label (.getColumnLabel rsmeta i)]
            (-> (case type
                  :bool (str label "?")
                  label)
                csk/->kebab-case-keyword)))
        (range 1 (inc (.getColumnCount rsmeta)))))

(defn as-sane-maps
  [^ResultSet rs opts]
  (let [rsmeta (.getMetaData rs)
        cols   (get-column-names rsmeta)]
    (rs/->MapResultSetBuilder rs rsmeta cols)))

(defn as-pg-map
  [m]
  (if (map? m)
    (cske/transform-keys csk/->snake_case_string m)
    ;; Otherwise it's probably a vector of [SQL-STRING params...]
    m))

(defn execute
  [sql-params]
  (jdbc/execute! *connection*
                 sql-params
                 {:builder-fn as-sane-maps}))

(defn execute-one
  "Takes and SQL string and parameters, morphs them into the shape
  required to execute a single SQL statement."
  [sql & params]
  (execute (into [sql] params)))

(defn rollback
  []
  (.rollback *transaction*))

(defn updated?
  [result]
  (cond
    (map? result) (some-> result :next.jdbc/update-count zero? not)
    :else (-> (some-> result
                      first
                      :next.jdbc/update-count
                      zero?
                      not)
              boolean)))

;;; --------------------------------------------------------------------------------

(defn insert-row
  [tablename record]
  (sql/insert! *connection* (csk/->snake_case_string tablename) (as-pg-map record)))

(defn update-rows
  "Woefully inefficient way of getting data but will suffice for now."
  [tablename record where]
  (sql/update! *connection* (csk/->snake_case_string tablename) (as-pg-map record) (as-pg-map where)))

(defn delete-rows
  "Woefully inefficient way of getting data but will suffice for now."
  [tablename where]
  (sql/delete! *connection* (csk/->snake_case_string tablename) (as-pg-map where)))

;;; --------------------------------------------------------------------------------

(defn get-identity
  [entity-id record]
  (or (some-> (os/rec-identity entity-id record)
              (record->row :domain    (get-table entity-id)
                           :db-names? true))
      (throw (ex-info (format "Failed to extract identity for %s" entity-id)
                      {:record record}))))

(defn get-values
  [entity-id record values extras]
  (some-> (merge (or values
                     (os/rec-values entity-id record))
                 extras)
          (record->row :domain    (get-table entity-id)
                       :db-names? true)))

;;;

(defn add-entity
  [entity-key record & {:keys [values extras debug?]}]
  (let [table  (get-table entity-key)
        token  (get-identity entity-key record)
        values (or (get-values   entity-key record values extras)
                   (throw (ex-info (format "Failed to extract values for %s" entity-key)
                                   {:entity-key entity-key
                                    :record     record
                                    :values     values
                                    :extras     extras})))]
    (when debug?
      (log/info (format "Add Entity for: `%s`" entity-key))
      (log/info (format "...table: %s" table))
      (log/info (format "...token: %s" token))
      (log/info (format "...values: %s" values)))
    (insert-row table
                (merge token values))))

(defn modify-entity
  [entity-key record & {:keys [values extras debug?]}]
  (let [table  (get-table entity-key)
        token  (get-identity entity-key record)
        values (or (get-values   entity-key record values extras)
                   (throw (ex-info (format "Failed to extract values for %s" entity-key)
                                   {:entity-key entity-key
                                    :record     record
                                    :values     values
                                    :extras     extras})))]
    (when debug?
      (log/info (format "Modify Entity for: `%s`" entity-key))
      (log/info (format "...table: %s" table))
      (log/info (format "...token: %s" token))
      (log/info (format "...values: %s" values)))
    (update-rows table values token)))

(defn rename-entity
  [entity-key record field-keys->new-field-keys]
  (update-rows (get-table    entity-key)
               (-> (->> field-keys->new-field-keys
                        (map (fn [[id-field-key new-value-key]]
                               [id-field-key (get record new-value-key)]))
                        (into {}))
                   (record->row :domain    (get-table entity-key)
                                :db-names? true))
               (get-identity entity-key record)))

(defn remove-entity
  [entity-id record]
  (delete-rows (get-table    entity-id)
               (get-identity entity-id record)))
