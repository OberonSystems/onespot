(ns onespot.honeysql
  (:require [oberon.utils :refer [prefix-keyword]]
            ;;
            [honey.sql         :as hsql]
            [honey.sql.helpers :as h]
            ;; Registers things like <@, etc.
            honey.sql.pg-ops
            ;;
            [onespot.core     :as os]
            [onespot.postgres :as pg]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp      (atom nil))
(defn-   ^:private set-tmp! [value] (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------

(defn execute-sql
  [sql & [debug?]]
  (let [[sql & params :as query] (hsql/format sql :pretty debug?)]
    (when debug?
      (println sql)
      (println params))
    (pg/execute query)))

;;; --------------------------------------------------------------------------------
;;  Honeysql Helpers

(defn like%
  [v]
  ;; FIXME: workout how to replace this with an extention to honeysql
  ;; so we can just use [:ilike% :field "value"] and it'll stick the
  ;; `%` in.
  (when v (str "%" v "%")))

;;; --------------------------------------------------------------------------------

;; I couldn't get :any to work consistently, if it is treated like a
;; function by HoneySQL then everything is fine, but sometimes it
;; wants to treat it like an operator.
;;
;; I've replaced usage of [:any ...] below with [:raw "ANY(...)"].

;;; --------------------------------------------------------------------------------

(def order-by* #(apply h/order-by %1 %2))
(def group-by* #(apply h/group-by %1 %2))

(defn select*
  ([prefix colls]
   (select* nil prefix colls))
  ([sql prefix colls]
   (->> colls
        (remove nil?)
        (map (fn [col]
               (cond
                 ;; Handle [:column :as-this-name]
                 (vector? col) (let [[col as] col]
                                 [(prefix-keyword prefix col)
                                  as])
                 ;; Otherwise we treat it like a keyword and let it
                 ;; blow up if it isn't a keyword.
                 :else (prefix-keyword prefix col))))
        (into (if sql [sql] []))
        (apply h/select))))

(defn page-by
  [sql {{:keys [index size]} :page}]
  (cond-> sql
    size (-> (h/offset (* size (or index 0)))
             (h/limit size))))

(defn row-count
  [sql {:keys [count?]}]
  (cond-> sql
    count? (-> (h/select [[:over [[:count :*]]]
                          :total]))))

(defn add-options
  [sql options]
  (-> sql
      (page-by   options)
      (row-count options)))

;;; --------------------------------------------------------------------------------
;;  It's quite common to keep tags in a TEXT[] in postgres, so we have
;;  some helpers here to keep them in sync.

(defn remove-tag
  [table column where tag-id & {:keys [array-type debug?]
                                :or   {array-type :text}}]
  (let [sql (-> (h/update table)
                (h/set {column [:remove_from_array column
                                 [:cast tag-id array-type]]})
                (h/where [:and where
                          ;; Should be able to use [:any :tags] below
                          ;; but there is a bug somewhere deep in
                          ;; HoneySQL related to treating :any as an
                          ;; operator.
                           [:= tag-id [:raw "ANY(tags)"]]]))]
    (execute-sql sql debug?)))

(defn rename-tag
  [table column where tag-id new-tag-id & {:keys [array-type debug?]
                                           :or   {array-type :text}}]
  (let [sql (-> (h/update table)
                (h/set {column [:replace_in_array column
                                [:cast tag-id     array-type]
                                [:cast new-tag-id array-type]]})
                (h/where [:and where
                          ;; See comment above.
                          [:= tag-id [:raw "ANY(tags)"]]]))]
    (execute-sql sql debug?)))

;;; --------------------------------------------------------------------------------

(defmulti fetch (fn [kind & [matching & {:as opts}]]
                  (or (when-let [entity (and (os/registered? kind)
                                             (os/pull kind))]
                        (or (-> entity ::info    :collection)
                            (-> entity ::pg/info :table)))
                      kind)))

(defn fetch-all
  [sql debug?]
  (execute-sql sql debug?))

(defn one
  [kind matching & {:keys [throw?] :as opts}]
  (let [opts (->> (-> opts
                      (assoc  :page {:size 2})
                      (dissoc :throw?))
                  (into (list))
                  flatten)
        records (apply fetch kind matching opts)]
    (if (= (count records) 1)
      (first records)
      (when throw?
        (throw (ex-info "Failed to find exactly 1 record."
                        {:kind kind :matching matching}))))))

;;; --------------------------------------------------------------------------------

(defn entity-present?
  [entity-id record & {:keys [matching debug?]}]
  (let [table    (pg/get-table entity-id)
        matching (merge (os/rec-identity entity-id record)
                        matching)]
    (one table matching :debug? debug?)))

(def entity-absent? (complement entity-present?))
