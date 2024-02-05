(ns onespot.json
  (:require [clojure.string :as s]
            [clojure.core.memoize :as m]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [oberon.utils :refer [map-entry]]
            ;;
            [onespot.core    :as osc]
            [onespot.lacinia :as osl])
  (:import [java.time Instant LocalDate]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp (atom nil))
(defn-   ^:private set-tmp!
  [value]
  (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------
;;  Memoized as recommended by CSK project.

(def ->camelCase
  (m/fifo csk/->camelCase {} :fifo/threshold 1024))

(def ->kebab-case-string
  (m/fifo csk/->kebab-case-string {} :fifo/threshold 1024))

(def ->kebab-case-keyword
  (m/fifo csk/->kebab-case-keyword {} :fifo/threshold 1024))

(def ->SCREAMING_SNAKE_CASE_STRING
  (m/fifo csk/->SCREAMING_SNAKE_CASE_STRING {} :fifo/threshold 1024))

(defn keys->camel-case
  [m]
  (cske/transform-keys ->camelCase m))

(defn keys->kebab-case
  [m]
  (cske/transform-keys ->kebab-case-keyword m))

;;; --------------------------------------------------------------------------------

(defn entity-id
  [entity-id]
  (-> entity-id osc/canonical-entity-id osc/pull ::entity-id))

(defn get-entity
  [entish]
  (-> entish
      osc/canonical-entity-id
      osc/pull))

;;; --------------------------------------------------------------------------------

(defn- kind-dispatcher
  [entity value]
  (let [osc-entity-id (osc/entity-id entity)]
    (or (-> entity ::info ::type)
        (when (and (osc/scalar? osc-entity-id)
                   (osc/enum?   osc-entity-id))
          ::enum)
        osc-entity-id)))

(defmulti entity->json kind-dispatcher)

(defmethod entity->json :default
  [entity value]
  ;;(println (kind-dispatcher entity value) entity value)
  (when-not (nil? value)
    (case (osc/kind entity)
      ::osc/scalar value
      ;;
      ::osc/attr   (let [{::keys [entity-id]} entity
                         osl-entity-id        (osl/entity-id entity)
                         osc-entity-id        (osc/entity-id entity)]
                     (map-entry (-> (or entity-id osl-entity-id osc-entity-id))
                                (entity->json (osc/attr-entity entity)
                                              (get value osc-entity-id))))
      ;;
      ::osc/rec    (->> (concat (osc/rec-attrs        entity)
                                (osl/rec-output-attrs entity))
                        (map #(entity->json % value))
                        (into {}))

      ::osc/series (let [series-entity (osc/series-entity entity)]
                     (mapv #(entity->json series-entity %)
                           value)))))

(defmethod entity->json ::osc/keyword
  [entity value]
  (some-> value ->kebab-case-string))

(defmethod entity->json ::osc/local-date
  [entity value]
  (some-> value .toString))

(defmethod entity->json ::osc/instant
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
    (case (osc/kind entity)
      ::osc/scalar value
      ;;
      ::osc/attr   (let [{::keys [entity-id]} entity
                         osl-entity-id        (osl/entity-id entity)
                         osc-entity-id        (osc/entity-id entity)]
                     (map-entry osc-entity-id
                                (json->entity (osc/attr-entity entity)
                                              (get value (or entity-id
                                                             osl-entity-id
                                                             osc-entity-id)))))
      ;;
      ::osc/rec    (->> (osc/rec-attrs entity)
                        (map #(json->entity % value))
                        (into {}))
      ;;
      ::osc/series (let [series-entity (osc/series-entity entity)]
                     (mapv #(json->entity series-entity %)
                           value)))))

(defmethod json->entity ::osc/keyword
  [entity value]
  (some-> value ->kebab-case-keyword))

(defmethod json->entity ::osc/local-date
  [entity value]
  (some-> value LocalDate/parse))

(defmethod json->entity ::osc/instant
  [entity value]
  (some-> value Instant/parse))

(defmethod json->entity ::enum
  [entity value]
  (some-> value ->kebab-case-keyword))

;;; --------------------------------------------------------------------------------

(defn write-json
  [entish value]
  (-> (get-entity entish)
      (entity->json value)))

(defn read-json
  [entish value]
  (-> (get-entity entish)
      (json->entity value)))
