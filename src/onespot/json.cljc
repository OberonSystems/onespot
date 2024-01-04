(ns onespot.json
  (:require [clojure.string :as s]
            [camel-snake-kebab.core :as csk]
            [oberon.utils :refer [map-entry]]
            ;;
            [onespot.core :as osc]))

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
  (-> value csk/->kebab-case-string))

(defmethod entity->json [::osc/scalar ::enum]
  [entity value]
  (-> value name csk/->SCREAMING_SNAKE_CASE_STRING))

(defmethod entity->json [::osc/attr ::default]
  [{::osc/keys [entity-id] :as entity} value]
  (map-entry (-> (or (entity ::entity-id) entity-id))
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
  (-> value csk/->kebab-case-keyword))

(defmethod json->entity [::osc/scalar ::enum]
  [entity value]
  (-> value csk/->kebab-case-keyword))

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
