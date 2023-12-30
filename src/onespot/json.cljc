(ns onespot.json
  (:require [clojure.string :as s]
            [onespot.utils :refer [map-entry]]
            [onespot.core :as osc]))

;;;

(defn- kind-dispatcher
  [{entity-kind ::osc/kind
    json-kind   ::kind} value]
  [entity-kind (or json-kind ::default)])

(defmulti entity->json kind-dispatcher)

(defmethod entity->json [::osc/scalar ::default]
  [entity value]
  ;; No coercion, assumes it's a native type that the json writer can
  ;; handle.
  value)

(defmethod entity->json [::osc/scalar :enum]
  [entity value]
  (-> value name s/upper-case))

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
