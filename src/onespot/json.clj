(ns onespot.json
  (:require [clojure.string :as s]
            [clojure.core.memoize :as m]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [oberon.utils :refer [map-entry]]
            ;;
            [onespot.core :as osc]))
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

(defn kind
  [entity]
  (::kind entity))

;;;

(defn- kind-dispatcher
  [entity value]
  [(osc/kind entity) (or (kind entity) ::default)])

(defmulti entity->json kind-dispatcher)

(defmethod entity->json [::osc/scalar ::default]
  [entity value]
  ;; No coercion, assumes it's a native type that the json writer can
  ;; handle.
  value)

(defmethod entity->json [::osc/scalar ::keyword]
  [entity value]
  (-> value ->kebab-case-string))

(defmethod entity->json [::osc/scalar ::enum]
  [entity value]
  (-> value name ->SCREAMING_SNAKE_CASE_STRING))


(defmethod entity->json [::osc/attr ::default]
  [{::osc/keys [entity-id]
    ::keys [json-id]
    :as entity} value]
  (map-entry (-> (or json-id entity-id))
             (entity->json (osc/attr-entity entity)
                           (get value entity-id))))

(defmethod entity->json [::osc/rec ::default]
  [entity value]
  (->> (osc/rec-attrs entity)
       (map #(entity->json % value))
       (into {})))

(defmethod entity->json [::osc/series ::default]
  [entity value]
  (let [series-entity (osc/series-entity entity)]
    (mapv #(entity->json series-entity %)
          value)))

;;; --------------------------------------------------------------------------------

(defmulti json->entity kind-dispatcher)

(defmethod json->entity [::osc/scalar ::default]
  [entity value]
  ;; No coercion, assumes it's a native type that is already a valid clojure value
  value)

(defmethod json->entity [::osc/scalar ::keyword]
  [entity value]
  (-> value ->kebab-case-keyword))

(defmethod json->entity [::osc/scalar ::enum]
  [entity value]
  (-> value ->kebab-case-keyword))


(defmethod json->entity [::osc/attr ::default]
  [{::osc/keys [entity-id]
    ::keys [json-id]
    :as entity} value]
  (map-entry entity-id
             (json->entity (osc/attr-entity entity)
                           (get value json-id
                                ;; Pass default value to `get` to
                                ;; ensure `false` is returned rather
                                ;; than (or ...).
                                (get value entity-id)))))

(defmethod json->entity [::osc/rec ::default]
  [entity value]
  (->> (osc/rec-attrs entity)
       (map #(json->entity % value))
       (into {})))

(defmethod json->entity [::osc/series ::default]
  [entity value]
  (let [series-entity (osc/series-entity entity)]
    (mapv #(json->entity series-entity %)
          value)))

;;; --------------------------------------------------------------------------------

(defn write-json
  [entish value]
  (let [entity (-> entish
                   osc/canonical-entity-id
                   osc/pull)]
    (entity->json entity value)))

(defn read-json
  [entish value]
  (let [entity (-> entish
                   osc/canonical-entity-id
                   osc/pull)]
    (json->entity entity value)))
