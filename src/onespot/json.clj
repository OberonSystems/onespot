(ns onespot.json
  (:require [clojure.string :as s]
            [clojure.core.memoize :as m]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [oberon.utils :refer [map-entry]]
            ;;
            [onespot.snakes :refer [->SCREAMING_SNAKE_CASE_STRING ->kebab-case-keyword ->kebab-case-string
                                    keys->kebab-case keys->camel-case]]
            [onespot.core   :as os])
  (:import [java.time Instant LocalDate]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp (atom nil))
(defn-   ^:private set-tmp!
  [value]
  (swap! tmp (constantly value)))

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
  [entity value]
  (let [os-entity-id (os/entity-id entity)]
    (or (-> entity ::info :type)
        (when (and (os/scalar? os-entity-id)
                   (os/enum?   os-entity-id))
          ::enum)
        os-entity-id)))

(defmulti entity->json kind-dispatcher)

(defmethod entity->json :default
  [entity value]
  ;;(println (kind-dispatcher entity value) entity value)
  (when-not (nil? value)
    (case (os/kind entity)
      :scalar value
      ;;
      :attr   (entity->json (os/attr-entity entity) value)
      ;;
      :rec    (->> (os/rec-attrs entity :readonly? true)
                   (map (fn [attr]
                          (let [entity-id (os/entity-id attr)]
                            [entity-id (entity->json attr (get value entity-id))])))
                   (into {}))

      :series (let [series-entity (os/series-entity entity)]
                (mapv #(entity->json series-entity %)
                      value)))))

(defmethod entity->json ::os/keyword
  [entity value]
  (some-> value ->kebab-case-string))

(defmethod entity->json ::os/local-date
  [entity value]
  (some-> value .toString))

(defmethod entity->json ::os/instant
  [entity value]
  (some-> value .toString))

(defmethod entity->json ::enum
  [entity value]
  (some-> value name ->SCREAMING_SNAKE_CASE_STRING))

;;; --------------------------------------------------------------------------------

(defmulti json->entity kind-dispatcher)

(defmethod json->entity :default
  [entity value]
  (when-not (nil? value)
    (case (os/kind entity)
      :scalar value
      ;;
      :attr   (json->entity (os/attr-entity entity) value)
      ;;
      :rec    (->> (os/rec-attrs entity)
                   (map (fn [attr]
                          (let [entity-id (os/entity-id attr)]
                            [entity-id (json->entity attr (get value entity-id))])))
                   (into {}))
      ;;
      :series (let [series-entity (os/series-entity entity)]
                (mapv #(json->entity series-entity %)
                      value)))))

(defmethod json->entity ::os/keyword
  [entity value]
  (some-> value ->kebab-case-keyword))

(defmethod json->entity ::os/local-date
  [entity value]
  (some-> value LocalDate/parse))

(defmethod json->entity ::os/instant
  [entity value]
  (some-> value Instant/parse))

(defmethod json->entity ::enum
  [entity value]
  (some-> value ->kebab-case-keyword))

;;; --------------------------------------------------------------------------------

(defn ->json-keys
  [m]
  (keys->camel-case m :rename-map (os/make-core-key->ns-key ::entity-id)))

(defn ->json
  ([entish value]
   (if-let [entity (when (os/registered? entish)
                     (get-entity entish))]
     (entity->json entity value)
     ;; We assume the native JSON coercion can handle it.
     value))
  ([m]
   (->> m
        (map (fn [[k v]]
               [k (->json k v)]))
        (into {}))))

;;;

(defn ->core-keys
  [m]
  (keys->kebab-case m :rename-map (os/make-ns-key->core-key ::entity-id)))

(defn ->core
  ([entish value]
   (if-let [entity (when (os/registered? entish)
                     (get-entity entish))]
     (json->entity entity value)
     ;; We assume the native JSON coercion can handle it.
     value))
  ([m]
   (->> m
        (map (fn [[k v]]
               [k (->core k v)]))
        (into {}))))
