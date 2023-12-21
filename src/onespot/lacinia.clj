(ns onespot.lacinia
  (:require [clojure.string :as s]
            [clojure.walk :refer [postwalk]]
            ;;
            [camel-snake-kebab.core :as csk :refer [->PascalCaseKeyword
                                                    ->camelCaseKeyword]]
            ;;
            [onespot.utils :refer [map-entry]]
            [onespot.core :as osc]))

;;; --------------------------------------------------------------------------------

(defn extract-map-entries
  [m extract?]
  (let [result (atom [])]
    (postwalk (fn [x]
                (if (and (map-entry? x)
                         (extract? x))
                  (swap! result conj x))
                x)
              m)
    @result))

;;; --------------------------------------------------------------------------------

(def +native-map+
  {:string  'String
   :boolean 'Boolean
   :int     'Int
   :float   'Float
   :id      'ID})

(defn native?
  [k]
  (contains? +native-map+ k))

(defn ->gql-type
  [field-type in-out many? optional?]
  {:pre [(#{:in :out} in-out)]}
  (let [gql-type (or (+native-map+ field-type)
                     (->PascalCaseKeyword (if in-out
                                            (str (name field-type) "-" (name in-out))
                                            field-type)))]
    (cond
      many? (let [term (list 'list
                             (list 'not-null gql-type))]
              (if optional?
                term
                (list 'not-null term)))
      :else (if optional?
              gql-type
              (list 'not-null gql-type)))))

;;; --------------------------------------------------------------------------------

(defn arg->field-ref
  "What do we support here?

  {:person-id  nil}
  {:person-id  {:type :string}}
  {:person-id  {:type :string :optional? true}}
  {:people-ids {:type [:string] :optional? true}}
  {:people-ids {:type [:string]}}

  {:people nil} <- :people is a osc/series
  {:people {:optional? true} <- :people is a osc/series

  If info is nil then we lookup the info from registry.

  If info is a map then we base it on the :type field in the map.
  "
  [arg-id {:keys [optional?] :as info}]
  (let [ ;; make-error (fn []
        ;;              (ex-info (format "Can't convert arg `%s` to a field-ref." arg-id)
        ;;                       {:arg-id arg-id :info info}))
        ;;
        entity-id  (or (and (-> info :type vector?) (-> info :type first))
                       (:type info)
                       arg-id)
        ;; Did the type information come from the info?
        info-type? (contains? info :type)
        ;;
        many?      (or (-> info :type vector?)
                       (osc/series? entity-id)
                       (and (osc/attr? entity-id)
                            (-> entity-id
                                osc/attr
                                osc/attr-type
                                osc/series?)))
        ;;
        optional?  (boolean optional?)]
    (cond
      (native? entity-id)
      {:entity     entity-id
       :clj-arg-id arg-id
       :gql-arg-id (->camelCaseKeyword arg-id)
       :gql-type   (->gql-type entity-id :in many? optional?)
       :many?      many?
       :optional?  optional?}
      ;;
      (osc/scalar? entity-id)
      (let [entity (osc/scalar entity-id)]
        {:entity    entity
         :clj-arg-id arg-id
         :gql-arg-id (->camelCaseKeyword arg-id)
         :gql-type   (->gql-type (or (entity ::gql-type)
                                     entity-id)
                                 :in many? optional?)
         :many?      many?
         :optional?  optional?})
      ;;
      (osc/rec? entity-id)
      (let [entity (osc/rec entity-id)]
        {:entity     entity
         :clj-arg-id arg-id
         :gql-arg-id (-> (or (when info-type? arg-id)
                             (entity ::gql-arg-id)
                             arg-id)
                         ->camelCaseKeyword)
         :gql-type   (->gql-type (or (entity ::gql-type)
                                     entity-id)
                                 :in many? optional?)
         :many?      many?
         :optional?  optional?})
      ;;
      (osc/series? entity-id)
      (let [entity      (osc/series entity-id)
            series-type (osc/series-type entity)]
        {:entity      entity
         :series-type series-type
         ;;
         :clj-arg-id  arg-id
         :gql-arg-id  (-> (or (when info-type? arg-id)
                              (entity ::gql-arg-id)
                              arg-id)
                          ->camelCaseKeyword)
         :gql-type    (->gql-type (or (series-type ::gql-type)
                                      (series-type ::osc/entity-id))
                                  :in many? optional?)
         :many?     many?
         :optional? optional?})
      ;;
      (osc/attr? entity-id)
      (let [entity    (osc/attr      entity-id)
            attr-type (osc/attr-type entity)]
        {:entity    entity
         :attr-type attr-type
         ;;
         :clj-arg-id  arg-id
         :gql-arg-id  (-> (or (when info-type? arg-id)
                              (entity ::gql-arg-id)
                              arg-id)
                          ->camelCaseKeyword)
         :gql-type    (->gql-type (or (attr-type ::gql-type)
                                      (attr-type ::osc/entity-id))
                                  :in many? optional?)}))))

(defn end-point-args
  [schema]
  (->> (extract-map-entries schema (fn [[k v]]
                                     (and (map? v)
                                          (contains? v :args))))
       (map (fn [[k v]]
              [k (->> v :args
                      (map (fn [[arg-id info]]
                             (arg->field-ref arg-id info))))]))
       (into {})))

;;; --------------------------------------------------------------------------------

(defn return-type->field-ref
  [return-type]
  (let [entity-id (if (vector? return-type)
                    (first return-type)
                    return-type)
        many?     (or (vector?     return-type)
                      (osc/series? return-type))]
    (cond
      (native? return-type)
      {:entity   entity-id
       :gql-type (->gql-type (+native-map+ entity-id) :out many? false)
       :many?    many?}
      ;;
      (osc/scalar? entity-id)
      (let [entity (osc/scalar entity-id)]
        {:entity   entity
         :gql-type (->gql-type (or (::gql-type entity)
                                   entity-id)
                               :out many? false)
         :many?    many?})
      ;;
      (osc/rec? entity-id)
      (let [entity (osc/rec entity-id)]
        {:entity   entity
         :gql-type (->gql-type (or (::gql-type entity)
                                   entity-id)
                               :out many? false)
         :many?    many?})
      ;;
      (osc/series? entity-id)
      (let [entity      (osc/series entity-id)
            series-type (osc/series-type entity)]
        (if many?
          ;; Then we are returning a list of lists
          (throw (ex-info "Sorry, not yet handling returning a list of lists."
                          {:return-type return-type}))
          {:entity   entity
           :gql-type (->gql-type (or (::gql-type      series-type)
                                     (::osc/entity-id series-type))
                                 :out true false)
           :many?    true}))
      ;;
      :else (throw (ex-info (format "Can't convert return-type `%s` to a field-ref." return-type)
                            {:return-type return-type})))))

(defn end-point-types
  [schema]
  (->> (extract-map-entries schema (fn [[k v]]
                                     (and (map? v)
                                          (contains? v :type))))
       (map second)
       (map :type)
       distinct
       (map (fn [type-key]
              [type-key (return-type->field-ref type-key)]))
       (into {})))

;;; --------------------------------------------------------------------------------

(defn end-point->gql
  [end-point-name
   {:keys [type args resolve] :as end-point}
   end-point-type-map
   end-point-arg-map]
  {:type (get end-point-type-map type)
   :args (->> args
              (map (fn [[k _]]
                     (let [{:keys [arg-name arg-spec] :as arg-type} (get end-point-arg-map [end-point-name k])]
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
