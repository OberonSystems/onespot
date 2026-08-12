(ns onespot.html
  (:require [clojure.string :as s]
            [onespot.base62 :as b62]
            [onespot.snakes :refer [->SCREAMING_SNAKE_CASE_STRING ->kebab-case-keyword ->kebab-case-string
                                    keys->ns keys->kebab-case]]
            [onespot.core   :as os])
  (:import [java.time Instant LocalDate]))

;;; --------------------------------------------------------------------------------

(defn get-entity-id
  [entity-id]
  (-> entity-id os/canonical-entity-id os/pull ::entity-id))

(defn get-entity
  [entish]
  (-> entish
      os/canonical-entity-id
      os/pull))

(defn get-label
  [entity-id]
  (when-let [entity-id (and (os/registered? entity-id)
                            (os/canonical-entity-id entity-id))]
    (or (-> entity-id os/pull ::info :label)
        (os/label entity-id))))

(defn get-input-type
  [entity-id]
  (when-let [entity-id (and (os/registered? entity-id)
                            (os/canonical-entity-id entity-id))]
    (cond
      (os/attr? entity-id)
      (or (-> entity-id os/pull ::info :input-type)
          (os/attr-entity-id entity-id))

      :else entity-id)))

(defn get-input-options
  [entity-id]
  (when-let [entity-id (and (os/registered? entity-id)
                            (os/canonical-entity-id entity-id))]
    (when (os/attr? ))
    (cond
      (os/attr? entity-id)
      (or (-> entity-id os/pull ::info :input-type)
          (os/attr-entity-id entity-id))

      :else entity-id)))

(defn get-help
  [entity-id]
  (when-let [entity-id (and (os/registered? entity-id)
                            (os/canonical-entity-id entity-id))]
    (-> entity-id os/pull ::info :help)))

;;; --------------------------------------------------------------------------------

(defn- kind-dispatcher
  [entity _value]
  (let [os-entity-id (os/entity-id entity)]
    (or (-> entity ::info :type)
        (when (and (os/scalar? os-entity-id)
                   (os/enum?   os-entity-id))
          ::enum)
        os-entity-id)))

(defmulti entity->html kind-dispatcher)

(defmethod entity->html :default
  [entity value]
  (when-not (nil? value)
    (case (os/kind entity)
      :scalar value ; everything is a string in html land
      :attr   (entity->html (os/attr-entity entity) value)
      :rec    (->> (os/rec-attrs entity :readonly? true)
                   (map (fn [attr]
                          (let [entity-id (os/entity-id attr)]
                            [entity-id (entity->html attr (get value entity-id))])))
                   (into {}))
      :series (let [series-entity (os/series-entity entity)]
                (mapv #(entity->html series-entity %)
                      value)))))

(defmethod entity->html ::os/boolean
  [_entity value]
  (if value
    "true"
    "false"))

(defmethod entity->html ::os/keyword
  [_entity value]
  (some-> value ->kebab-case-string))

(defmethod entity->html ::os/local-date
  [_entity value]
  (some-> value .toString))

(defmethod entity->html ::os/instant
  [_entity value]
  (some-> value .toString))

(defmethod entity->html ::os/big-decimal
  [_entity value]
  (some-> value .toString))

(defmethod entity->html ::enum
  [_entity value]
  (some-> value name ->SCREAMING_SNAKE_CASE_STRING))

;;; --------------------------------------------------------------------------------

(defmulti html->entity kind-dispatcher)

(defmethod html->entity :default
  [entity value]
  (when-not (nil? value)
    (case (os/kind entity)
      :scalar value
      :attr   (html->entity (os/attr-entity entity) value)
      :rec    (->> (os/rec-attrs entity)
                   (map (fn [attr]
                          (let [entity-id (os/entity-id attr)]
                            [entity-id (html->entity attr (get value entity-id))])))
                   (into {}))
      :series (let [series-entity (os/series-entity entity)]
                (mapv #(html->entity series-entity %)
                      value)))))

(defmethod html->entity ::os/boolean
  [_entity value]
  (when-let [value (some-> value
                           s/trim
                           s/lower-case)]
    (cond
      (= value "true") true
      :else false)))

(defmethod html->entity ::os/keyword
  [_entity value]
  (some-> value ->kebab-case-keyword))

(defmethod html->entity ::os/local-date
  [_entity value]
  (some-> value LocalDate/parse))

(defmethod html->entity ::os/instant
  [_entity value]
  (some-> value Instant/parse))

(defmethod html->entity ::os/positive-integer
  [_entity value]
  (some-> value parse-long))

(defmethod html->entity ::os/big-decimal
  [_entity value]
  (some-> value bigdec))

(defmethod html->entity ::os/alpha-numeric
  [_entity value]
  (let [value (some-> value (s/replace #"[^a-zA-Z0-9]" ""))]
    (when-not (s/blank? value)
      value)))

(defmethod html->entity ::os/e164
  [_entity value]
  (let [value (some-> value (s/replace #"[^+0-9]" ""))]
    (when-not (s/blank? value)
      value)))

(defmethod html->entity ::enum
  [_entity value]
  (some-> value ->kebab-case-keyword))

;;; --------------------------------------------------------------------------------

(defn ->html-keys
  [m]
  (keys->ns m :rename-map (os/make-core-key->ns-key ::entity-id)))

(defn ->html-value
  ([entish value]
   (if-let [entity (when (os/registered? entish)
                     (get-entity entish))]
     (entity->html entity value)
     value))
  ([value]
   (let [->value (fn [m]
                   (->> m
                        (map (fn [[k v]] [k (->html-value k v)]))
                        (into {})))]
     (cond
       (map?    value) (->value value)
       (vector? value) (mapv ->html-value value)
       (list?   value) (map  ->html-value value)))))

;;;

(defn ->clj-keys
  [m]
  (keys->kebab-case m :rename-map (os/make-ns-key->core-key ::entity-id)))

(defn ->clj-value
  ([entish value]
   (if-let [entity (when (os/registered? entish)
                     (get-entity entish))]
     (html->entity entity value)
     ;; We assume the native html coercion can handle it.
     value))
  ([value]
   (let [->value (fn [m]
                   (->> m
                        (map (fn [[k v]]
                               [k (->clj-value k v)]))
                        (into {})))]
     (cond
       (map?    value) (->value value)
       (vector? value) (mapv ->value value)
       (list?   value) (map  ->value value)))))

;;;

(defn ->html
  [value]
  (-> value
      ->html-value
      ->html-keys))

;;;

(defn find-prefixed
  [entity-id paths]
  (let [prefix (-> (or (get-entity-id entity-id)
                       entity-id)
                   name)]
    (->> paths
         (map (fn [[path value]]
                (when (= prefix (first path))
                  [(rest path) value])))
         (remove nil?))))

(defn ->entity
  [entity-id paths]
  (case (os/kind entity-id)
    :scalar (html->entity entity-id
                          (-> paths first second))
    :attr   (->> paths
                 (find-prefixed entity-id)
                 (->entity (os/attr-entity-id entity-id)))
    :rec    (->> (os/rec-attr-ids entity-id)
                 (map (fn [entity-id]
                        [entity-id (->entity entity-id paths)]))
                 (into {}))
    :series (let [series-entity-id (os/series-entity-id entity-id)]
              (->> paths
                   ;; Extract the first element of the path
                   (map (fn [[path value]]
                          [(first path)
                           [(rest path) value]]))
                   ;; Make sure they get returned in the order they were generated.
                   (sort-by      first)
                   (partition-by first)
                   (map (fn [indexed-paths]
                          (->> indexed-paths
                               (map second)
                               (->entity series-entity-id))))))))

; --------------------------------------------------------------------------------
; FIXME: add a path macro builder or some such

(defn compute-attr-name-map
  []
  (->> (os/attrs)
       (map (fn [attr]
              (let [entity-id    (os/entity-id  attr)
                    ns-entity-id (get-entity-id attr)]
                [(-> (or ns-entity-id entity-id) name)
                 entity-id])))
       (into {})))

(defn ->clj
  ([params] (->clj nil params))
  ([attr-ids params]
   (let [attrs (compute-attr-name-map)]
     (some->> params
              (remove #(-> % second s/blank?))
              (map (fn [[k v]]
                     (let [[head & _ :as path] (s/split k #";")
                           attr-id (attrs head)]
                       (when (and attr-id
                                  (or (not attr-ids)
                                      (contains? attr-ids attr-id)))
                         [attr-id [path v]]))))
              (remove nil?)
              seq
              (sort-by      first)
              (partition-by first)
              (map (fn [paths]
                     (let [entity-id (-> paths first first)]
                       [entity-id
                        (->> paths
                             (map second)
                             (->entity entity-id))])))
              (into {})))))

; --------------------------------------------------------------------------------

(def ^:dynamic *nodes* nil)
(def ^:dynamic *index* nil)

(defmulti node-name
  (fn [node]
    (type node)))

(defmethod node-name clojure.lang.Keyword
  [node]
  (if (os/attr? node)
    (-> (or (get-entity-id node)
            (os/entity-id node))
        name)
    (throw (ex-info (format "Node is not an attribute %s" node)
                    {:node node}))))

(defmethod node-name clojure.lang.Atom
  [node]
  (b62/encode @node))

(defmacro with-node
  [node & body]
  `(binding [*nodes* (conj *nodes* ~node)]
     ~@body))

(defmacro with-indexed
  [& body]
  `(binding [*index* (atom -1)]
     (with-node *index*
       ~@body)))

(defmacro with-index
  [& body]
  `(do
     (swap! *index* inc)
     ~@body))

(defn path
  [& [node]]
  (some->> (if node
             (conj *nodes* node)
             *nodes*)
           (map node-name)
           reverse
           (s/join ";")
           doall))
