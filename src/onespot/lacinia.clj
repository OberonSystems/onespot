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

(defn clj-name->gql-type-name
  [clj-name in-out]
  {:pre [(#{:in :out} in-out)]}
  (->PascalCaseKeyword (str (name clj-name) "-" (name in-out))))

(defn clj-name->gql-name
  [clj-name]
  (->camelCaseKeyword clj-name))

(defn ->gql-type
  [field-type in-out many? optional?]

  (let [gql-type (or (+native-map+ field-type)
                     (clj-name->gql-type-name field-type in-out))]
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
                                  :in many? optional?)})
      ;;
      (native? entity-id)
      {:entity     entity-id
       :clj-arg-id arg-id
       :gql-arg-id (->camelCaseKeyword arg-id)
       :gql-type   (->gql-type entity-id :in many? optional?)
       :many?      many?
       :optional?  optional?}
      ;;
      :else (throw (ex-info (format "Can't convert arg-id `%s` to a field-ref." arg-id)
                            {:arg-id arg-id})))))

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
        ;; FIXME: How do we handle it if the they are returing `many`
        ;; `serieses`?
        (if (vector? return-type)
          (throw (ex-info "Sorry, not yet handling returning a list of `series`."
                          {:return-type return-type})))
        ;;
        {:entity   entity
         :gql-type (->gql-type (or (::gql-type      series-type)
                                   (::osc/entity-id series-type))
                               :out true false)
         :many?    true})
      ;;
      (native? entity-id)
      {:entity   entity-id
       :gql-type (->gql-type entity-id :out many? false)
       :many?    many?}
      ;;
      :else (throw (ex-info (format "Can't convert return-type `%s` to a field-ref." return-type)
                            {:return-type return-type})))))

(defn return-types
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

(defn entity->field-ref
  [entity-id in-out optional?]
  (cond
    (osc/scalar? entity-id)
    (let [entity (osc/scalar entity-id)]
      {:type (->gql-type (or (::gql-type      entity)
                             (::osc/entity-id entity))
                         in-out false optional?)})
    ;;
    (osc/rec? entity-id)
    (let [entity (osc/rec entity-id)]
      {:type (->gql-type (or (::gql-type entity)
                             (::osc/entity-id entity))
                         in-out false optional?)})
    ;;
    (osc/series? entity-id)
    (let [entity      (osc/series entity-id)
          series-type (osc/series-type entity)]
      {:type (->gql-type (or (::gql-type      series-type)
                             (::osc/entity-id series-type))
                         in-out true optional?)})
    ;;
    :else (throw (ex-info (format "Can't convert entity `%s` to a field-ref." entity-id)
                          {:entity-id entity-id}))))

(defn rec->object
  [{::osc/keys [entity-id] :as rec}
   in-out]
  [(clj-name->gql-type-name entity-id in-out)
   {:fields   (->> (osc/rec-attrs rec)
                   (map (fn [attr]
                          (let [attr-type (osc/attr-type attr)]
                            [(clj-name->gql-name (or (::gql-id        attr)
                                                     (::osc/entity-id attr)))
                             (entity->field-ref attr-type
                                                in-out
                                                (osc/optional? rec attr))])))
                   (into {}))}])
