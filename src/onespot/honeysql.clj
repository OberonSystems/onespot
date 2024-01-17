(ns onespot.honeysql
  (:require [oberon.utils :refer [prefix-keyword]]
            ;;
            [honey.sql         :as hsql]
            [honey.sql.helpers :as h]
            ;;
            [onespot.core     :as osc]
            [onespot.postgres :as db]))

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
    (db/execute query)))

;;; --------------------------------------------------------------------------------
;;  Honeysql Helpers

(defn like%
  [v]
  ;; FIXME: workout how to replace this with an extention to honeysql
  ;; so we can just use [:ilike% :field "value"] and it'll stick the
  ;; % in.
  (str "%" v "%"))

;;; --------------------------------------------------------------------------------

(def order-by* #(apply h/order-by %1 %2))
(def group-by* #(apply h/group-by %1 %2))

(defn select*
  ([prefix colls]
   (select* nil prefix colls))
  ([sql prefix colls]
   (->> colls
        (map #(prefix-keyword prefix %))
        (into (if sql [sql] []))
        (apply h/select))))

(defn page-by
  [sql {{:keys [index size]} :paging}]
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
                           [:= tag-id [:any :tags]]]))]
    (execute-sql sql debug?)))

(defn rename-tag
  [table column where tag-id new-tag-id & {:keys [array-type debug?]
                                           :or   {array-type :text}}]
  (let [sql (-> (h/update table)
                (h/set {column [:replace_in_array column
                                [:cast tag-id     array-type]
                                [:cast new-tag-id array-type]]})
                (h/where [:and where
                          [:= tag-id [:any :tags]]]))]
    (execute-sql sql debug?)))

;;; --------------------------------------------------------------------------------

(defmulti fetch (fn [kind & [matching & {:as opts}]]
                  kind))

(defn fetch-all
  [sql debug?]
  (-> (execute-sql sql debug?)
      doall))

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
  (let [table    (db/get-table entity-id)
        matching (merge (osc/rec-identity entity-id record)
                        matching)]
    (one table matching :debug? debug?)))

(def entity-absent? (complement entity-present?))
