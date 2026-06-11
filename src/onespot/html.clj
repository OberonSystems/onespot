(ns onespot.html
  (:require [clojure.string :as s]
            [onespot.snakes :refer [->SCREAMING_SNAKE_CASE_STRING ->kebab-case-keyword ->kebab-case-string
                                    keys->ns keys->kebab-case]]
            [onespot.core   :as os])
  (:import [java.time Instant LocalDate]))

;;; --------------------------------------------------------------------------------

(defn entity-id
  [entity-id]
  (-> entity-id os/canonical-entity-id os/pull ::entity-id))

(defn get-entity
  [entish]
  (-> entish
      os/canonical-entity-id
      os/pull))

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
  ;(println (kind-dispatcher entity value) entity value)
  (when-not (nil? value)
    (case (os/kind entity)
      :scalar value
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
  (let [value (-> value
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

(defn ->clj
  [value]
  (-> value
      ->clj-keys
      ->clj-value))
