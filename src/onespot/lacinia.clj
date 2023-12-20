(ns onespot.lacinia
  (:require [clojure.string :as s]
            [clojure.walk :refer [postwalk]]
            ;;
            [onespot.utils :refer [map-entry]]
            [onespot.core :as osc]))

;;; --------------------------------------------------------------------------------

(def +native-map+
  {:string  'String
   :boolean 'Boolean
   :int     'Int
   :integer 'Int
   :float   'Float
   :id      'ID})

(defn native?
  [k]
  (contains? +native-map+ k))

;;; --------------------------------------------------------------------------------
;;  Functions to extra interesting bits from the schema.

(defn extract-key-values
  [m k]
  (let [result (atom [])]
    (postwalk (fn [x]
                (if (and (map-entry? x)
                         (-> x first (= k)))
                  (swap! result conj (second x)))
                x)
              m)
    @result))

(defn input-types
  [schema]
  (let [top (->> (extract-key-values schema :args)
                 (mapcat (fn [args]
                           (map (fn [[k v]]
                                  ;; We accept the following
                                  ;; - {:name nil}
                                  ;; - {:name {:type :some-type}}
                                  ;; - {:name :some-type}}
                                  (cond
                                    (nil?     v) k
                                    (keyword? v) v
                                    (and (map? v) (:type v)) (:type v)
                                    :else (throw (ex-info (format "Can't extract arg type from %s." [k v])
                                                          {:k k :v v}))))
                                args)))
                 distinct)]
    (->> top
         ;; Need a process to walk the top level recs and add
         ;; referenced values, they can be nested.
         ;;
         ;; We can walk the entities later and get all referenced
         ;; types.
         sort)))

;;; --------------------------------------------------------------------------------

(defn return-type->field-ref
  [return-type]
  (let [many? (or (vector?     return-type)
                  (osc/series? return-type))
        ;;
        return-type (if (vector? return-type)
                      (first return-type)
                      return-type)]
    (cond
      (native? return-type)
      {:native-type return-type
       :many?       many?}
      ;;
      (osc/scalar? return-type)
      {:entity (osc/scalar return-type)
       :many?  many?}
      ;;
      (osc/rec? return-type)
      {:entity (osc/rec return-type)
       :many?  many?}
      ;;
      (osc/series? return-type)
      {:entity (osc/series-type return-type)
       :many?  many?}
      ;;
      :else (throw (ex-info (format "Can't convert return-type `%s` to a field-ref." return-type)
                            {:return-type return-type})))))

(defn maybe-wrap-in-list
  [return-type many?]
  (if many?
    (list 'not-null
          (list 'list
                (list 'not-null return-type)))
    (list 'not-null return-type)))

(defmulti field-ref->end-point-type (fn [{:keys [native-type entity]}]
                                      (if native-type
                                        :native
                                        (::osc/kind entity))))

(defmethod field-ref->end-point-type :native
  [{:keys [native-type many?]}]
  (maybe-wrap-in-list (get +native-map+ native-type)
                      many?))

(defmethod field-ref->end-point-type ::osc/scalar
  [{{::osc/keys [entity-id] :as entity} :entity
    {::keys [gql-id]}                   :entity
    ;;
    :keys [many?]}]
  (maybe-wrap-in-list (or gql-id entity-id) many?))

(defmethod field-ref->end-point-type ::osc/rec
  [{{::osc/keys [entity-id] :as entity} :entity
    {::keys [gql-id]}                   :entity
    ;;
    :keys [many?]}]
  (maybe-wrap-in-list (-> (or gql-id entity-id)
                          name
                          (str "-out")
                          keyword)
                      many?))

(defmethod field-ref->end-point-type ::osc/series
  [{:keys [entity many?]}]
  ;; Series can't contain native types.
  (field-ref->end-point-type {:entity (osc/series-type entity)
                              :many?  true}))

(defn end-point-types
  [schema]
  (->> (extract-key-values schema :type)
       distinct
       (map (fn [k]
              (let [field-ref (return-type->field-ref k)]
                (merge field-ref
                       {:type-key  k
                        :type-spec (field-ref->end-point-type field-ref)}))))))

;;; --------------------------------------------------------------------------------

(defn query->gql
  [{:keys [type args resolve] :as end-point}
   end-point-type-map
   end-point-arg-map]
  {:type (get end-point-type-map type)
   :args (->> args
              (map (fn [[k _]]
                     (let [{:keys [arg-name arg-spec] :as arg-type} (get end-point-arg-map k)]
                       [arg-name arg-spec])))
              (into {}))}

  ;; (let [return-type (gql-type type)]
  ;;   {:type    (if (manyable? type)
  ;;               (list 'list (list 'not-null return-type))
  ;;               return-type)
  ;;    :args
  ;;    :resolve resolve})
  )

;;; --------------------------------------------------------------------------------
;;  Conversion Functions

(defn- kind-dispatcher
  [{entity-kind ::osc/kind
    gql-kind   ::kind} value]
  [entity-kind (or gql-kind ::default)])

(defmulti entity->gql kind-dispatcher)

(defmethod entity->gql [::osc/scalar ::default]
  [entity value]
  ;; No coercion, assumes it's a native type that the gql writer can
  ;; handle.
  value)

(defmethod entity->gql [::osc/scalar :enum]
  [entity value]
  (-> value name s/upper-case))

(defmethod entity->gql [::osc/attr ::default]
  [{::osc/keys [entity-id] :as entity} value]
  (map-entry (-> (or (entity ::entity-id) entity-id))
             (entity->gql (osc/attr-type entity)
                           (get value entity-id))))

(defmethod entity->gql [::osc/rec ::default]
  [entity value]
  (->> (osc/rec-attrs entity)
       (map #(entity->gql % value))
       (into {})))

(defmethod entity->gql [::osc/series ::default]
  [entity value]
  (let [series-type (osc/series-type entity)]
    (mapv #(entity->gql series-type %)
          value)))

;;; --------------------------------------------------------------------------------

(defmulti gql->entity kind-dispatcher)

(defn write-gql
  [entish value]
  (let [entity (-> entish
                   osc/canonical-entity-id
                   osc/pull)]
    (entity->gql entity value)))

(defn read-gql
  [entish value]
  (let [entity (-> entish
                   osc/canonical-entity-id
                   osc/pull)]
    (gql->entity entity value)))
