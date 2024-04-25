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
            [oberon.utils :refer [dump-> dump->> nil-when->>]]
            [onespot.cache :as cc]
            [onespot.core  :as os]
            [onespot.snakes :refer [->kebab-case-keyword ->snake_case_keyword ->snake_case_string ->SCREAMING_SNAKE_CASE_STRING]])
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

(defn ld->local-date-time
  [^LocalDate ld]
  (.format ld DateTimeFormatter/ISO_LOCAL_DATE_TIME))

(defn ld->offset-date-time
  [^LocalDate ld]
  (.format ld DateTimeFormatter/ISO_OFFSET_DATE_TIME))

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

(def make-enum
  (let [f (fn [enum-type enum-value]
            (when enum-value
              (make-pg-object (->SCREAMING_SNAKE_CASE_STRING enum-type) (name enum-value))))]
    (m/fifo f {} :fifo/threshold 1024)))

(defn make-daterange
  [[from to]]
  ;; Assumes that from to are inclusive, so we use [], postgres
  ;; defaults to [).
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
  ;;
  ;; We convert back to [] which is the inverse of the make-daterange
  ;; function.
  (let [[from to] (-> pgobj
                      get-pg-value
                      (s/split #","))
        from (some->> from (drop 1) seq (apply str) yyyy-mm-dd->ld)
        to   (some-> (some->> to butlast seq (apply str) yyyy-mm-dd->ld)
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

(defmethod entity->db ::ltree
  [entity v]
  (make-ltree v))

(defmethod entity->db ::enum
  [{::keys [entity-id info] :as entity} v]
  (make-enum (or (:enum-type info)
                 entity-id
                 (os/entity-id entity))
             v))

(defmethod entity->db ::keyword-array
  [entity v]
  (->> v
       (map name)
       (make-pg-array "TEXT")))

(defmethod entity->db ::text-array
  [entity v]
  (make-pg-array "TEXT" v))

(defmethod entity->db ::int-array
  [entity v]
  (make-pg-array "INT" v))

(defmethod entity->db ::date-array
  [entity v]
  (make-pg-array "DATE" (map clj->db v)))

(defmethod entity->db ::date-range
  [entity {:keys [date-from date-to]}]
  (make-daterange [date-from date-to]))

(defmethod entity->db ::instant-array
  [entity v]
  ;; FIXME:: Should these be TIMESTAMPS?
  (make-pg-array "TIMESTAMPTZ" (map clj->db v)))

(defmethod entity->db ::edn-map
  [entity v]
  (pr-str v))

;;;

(defn record->row
  [record & {:keys [domain db-names?]}]
  (->> record
       (map (fn [[k v]]
              (cond
                ;;
                ;; If it's an attr then we use the attr's entity to do
                ;; the conversion.  Attrs are `named` things that
                ;; point to another type of entity so they don't have
                ;; any ::info options themselves.
                (and (os/registered? k)
                     (os/attr?       k))
                (let [entity       (os/attr k)
                      entity-id    (::entity-id entity)
                      os-entity-id (os/entity-id entity)]
                  [(if db-names?
                     (-> (or entity-id os-entity-id) ->snake_case_keyword)
                     os-entity-id)
                   ;; If the attr-entity isn't specialised for ::kind
                   ;; then the native coercion will be called.
                   (when-not (nil? v)
                     (entity->db (os/attr-entity os-entity-id) v))])
                ;;
                :else [(if db-names?
                         (->snake_case_keyword k)
                         k)
                       (clj->db v)])))
       (into {})))

;;; --------------------------------------------------------------------------------

(defmulti db->clj (fn [v]
                    (type v)))

(defmethod db->clj :default
  [v]
  v)

(defmethod db->clj org.postgresql.jdbc.PgArray
  [v]
  (.getArray v))

;;;

(defmulti db->entity (fn [{:keys [entity-id] ::keys [info] :as entity} v]
                       (or (:type info) entity-id)))

(defmethod db->entity :default
  [entity v]
  (db->clj v))

(defmethod db->entity ::os/keyword
  [entity v]
  (keyword v))

(defmethod db->entity ::os/edn-map
  [entity v]
  (when v (edn/read-string v)))

(defmethod db->entity ::enum
  [entity v]
  (keyword v))

(defmethod db->entity ::keyword-array
  [entity v]
  (->> (.getArray v)
       (map keyword)))

(defmethod db->entity ::date-range
  [entity v]
  (let [[date-from date-to] v]
    {:date-from date-from
     :date-to   date-to}))

(defmethod entity->db ::edn-map
  [entity v]
  (edn/read-string v))

(defn make-row->record-attr-map
  "When the entity has a different entity-id in the `postgres` world we
  map it to the onespot entity-id so that we can correctly do the
  translations and coercions later on."
  []
  (->> (os/attrs)
       (map (fn [attr]
              (let [entity-id    (entity-id    attr)
                    os-entity-id (os/entity-id attr)]
                (when (and entity-id
                           os-entity-id
                           (not= entity-id os-entity-id))
                  [entity-id os-entity-id]))))
       (into {})))

(defn row->record
  [attr-map row]
  (->> row
       (map (fn [[k v]]
              (let [entity-id (get attr-map k k)]
                (cond
                  (and (os/registered? entity-id)
                       (os/attr?       entity-id))
                  (let [attr        (os/attr entity-id)
                        attr-entity (os/attr-entity attr)]
                    [entity-id
                     (some->> v (db->entity attr-entity))])
                  ;;
                  :else [entity-id
                         (some->> v db->clj)]))))
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

(defmethod read-object :daterange
  [_ v]
  (read-daterange v))

(let [object-reader (fn [object]
                      (read-object (-> object get-pg-type keyword) object))]
  (extend-protocol rs/ReadableColumn
    PGobject
    (read-column-by-label [^PGobject object _]          (object-reader object))
    (read-column-by-index [^PGobject object rsmeta idx] (object-reader object))))

;;; --------------------------------------------------------------------------------

(defn get-column-names
  [^ResultSetMetaData rsmeta]
  (mapv (fn [^Integer i]
          (-> (.getColumnLabel rsmeta i)
              ->kebab-case-keyword))
        (range 1 (inc (.getColumnCount rsmeta)))))

(defn as-sane-maps
  [^ResultSet rs opts]
  (let [rsmeta (.getMetaData rs)
        cols   (get-column-names rsmeta)]
    (rs/->MapResultSetBuilder rs rsmeta cols)))

(defn execute
  ([sql-params]
   (let [sql-params (if (string? sql-params)
                      [sql-params]
                      sql-params)
         attr-map   (cc/pull ::row->record-attr-map make-row->record-attr-map)]
     (->> (jdbc/execute! *connection*
                         sql-params
                         {:builder-fn as-sane-maps})
          (mapv #(row->record attr-map %))
          (nil-when->> empty?))))
  ([sql & params]
   (execute (into [sql] params))))

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
  (sql/insert! *connection*
               (->snake_case_string tablename)
               (record->row record :db-names? true)))

(defn update-rows
  [tablename record where]
  (sql/update! *connection*
               (->snake_case_string tablename)
               (record->row record :db-names? true)
               (record->row where  :db-names? true)))

(defn delete-rows
  [tablename where]
  (sql/delete! *connection*
               (->snake_case_string tablename)
               (record->row where :db-names? true)))

;;; --------------------------------------------------------------------------------

(defn add-entity
  [entity-id record & {:keys [values extras debug?]}]
  (let [table  (get-table entity-id)
        token  (os/rec-identity entity-id record)
        values (merge (or values (os/rec-values entity-id record :throw? true))
                      extras)]
    (when debug?
      (log/info (format "Add Entity for: `%s`" entity-id))
      (log/info (format "...table: %s" table))
      (log/info (format "...token: %s" token))
      (log/info (format "...values: %s" values)))
    (insert-row table (merge token values))))

(defn modify-entity
  [entity-id record & {:keys [values extras debug?]}]
  (let [table  (get-table entity-id)
        token  (os/rec-identity entity-id record)
        values (merge (or values (os/rec-values entity-id record :throw? true))
                      extras)]
    (when debug?
      (log/info (format "Modify Entity for: `%s`" entity-id))
      (log/info (format "...table: %s" table))
      (log/info (format "...token: %s" token))
      (log/info (format "...values: %s" values)))
    (update-rows table values token)))

(defn rename-entity
  [entity-id record field-keys->new-field-keys]
  (update-rows (get-table    entity-id)
               (-> (->> field-keys->new-field-keys
                        (map (fn [[id-field-key new-value-key]]
                               [id-field-key (get record new-value-key)]))
                        (into {}))
                   (record->row :domain    (get-table entity-id)
                                :db-names? true))
               (os/rec-identity entity-id record)))

(defn remove-entity
  [entity-id record]
  (delete-rows (get-table entity-id)
               (os/rec-identity entity-id record)))
