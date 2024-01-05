(ns onespot.lacinia
  (:require [clojure.string :as s]
            [clojure.walk :refer [postwalk]]
            [camel-snake-kebab.core :as csk :refer [->PascalCaseKeyword
                                                    ->camelCaseKeyword]]
            [oberon.utils :refer [nil-when->> map-entry hash-map*]]
            ;;
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
  {:pre [(#(contains? #{:in :out nil} in-out))]}
  (if in-out
    (->PascalCaseKeyword (str (name clj-name) "-" (name in-out)))
    (->PascalCaseKeyword clj-name)))

(defn clj-name->gql-name
  [clj-name]
  (->camelCaseKeyword clj-name))

(defn ->gql-type
  [field-type in-out many? optional?]

  (let [gql-type (or (+native-map+ field-type)
                     (clj-name->gql-type-name field-type in-out))]
    (cond
      many? (let [term (list 'list
                             (list 'non-null gql-type))]
              (if optional?
                term
                (list 'non-null term)))
      :else (if optional?
              gql-type
              (list 'non-null gql-type)))))

;;; --------------------------------------------------------------------------------

(defn arg->field-ref
  "What do we support here?

  {:person-id  nil}
  {:record     :some-entity-id}
  {:record     [:some-entity-id]}
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
  (let [info-type? (and (map? info) (contains? info :type)) ; Did the type information come from the info?
        optional?  (boolean optional?)
        ;;
        info (cond
               (nil? info)     {:type arg-id}
               ;;
               (keyword? info) {:type info}
               ;;
               (vector? info)  {:type (first info) :many? true}
               ;;
               (and (map? info)
                    (contains? info :type)
                    (-> info :type vector?)
                    (-> info :type first keyword?))
               {:type (-> info :type first) :many? true}
               ;;
               (and (map? info)
                    (contains? info :type)
                    (-> info :type keyword?))
               {:type (-> info :type)}
               ;;
               :else (ex-info (format "Can't convert arg `%s` to a field-ref." arg-id)
                              {:arg-id arg-id :info info}))
        ;;
        entity-id (:type info)
        many?     (or (-> info :many?)
                      (osc/series? entity-id)
                      (and (osc/attr? entity-id)
                           (-> entity-id
                               osc/attr
                               osc/attr-entity
                               osc/series?)))]
    (cond
      (osc/scalar? entity-id)
      (let [entity (osc/scalar entity-id)]
        {:entity-id  entity-id
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
        {:entity-id  entity-id
         :clj-arg-id arg-id
         :gql-arg-id (-> (or (when info-type? arg-id)
                             (entity ::gql-id)
                             arg-id)
                         ->camelCaseKeyword)
         :gql-type   (->gql-type (or (entity ::gql-type)
                                     entity-id)
                                 :in many? optional?)
         :many?      many?
         :optional?  optional?})
      ;;
      (osc/series? entity-id)
      (let [entity        (osc/series entity-id)
            series-entity (osc/series-entity entity)]
        {:entity-id        entity-id
         :series-entity-id (::osc/entity-id series-entity)
         ;;
         :clj-arg-id  arg-id
         :gql-arg-id  (-> (or (when info-type? arg-id)
                              (entity ::gql-id)
                              arg-id)
                          ->camelCaseKeyword)
         :gql-type    (->gql-type (or (series-entity ::gql-type)
                                      (series-entity ::osc/entity-id))
                                  :in many? optional?)
         :many?     many?
         :optional? optional?})
      ;;
      (osc/attr? entity-id)
      (let [entity      (osc/attr        entity-id)
            attr-entity (osc/attr-entity entity)]
        {:entity-id      entity-id
         :attr-entity-id (::osc/attr-entity-id entity)
         ;;
         :clj-arg-id  arg-id
         :gql-arg-id  (-> (or (when info-type? arg-id)
                              (entity ::gql-id)
                              arg-id)
                          ->camelCaseKeyword)
         :gql-type    (->gql-type (or (attr-entity ::gql-type)
                                      (attr-entity ::osc/entity-id))
                                  :in many? optional?)})
      ;;
      (native? entity-id)
      {:entity-id  entity-id
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
              [k (->> v
                      :args
                      (mapv (fn [[arg-id info]]
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
        {:entity-id entity-id
         :gql-type  (->gql-type (or (::gql-type entity)
                                    entity-id)
                                :out many? false)
         :many?     many?})
      ;;
      (osc/rec? entity-id)
      (let [entity (osc/rec entity-id)]
        {:entity-id entity-id
         :gql-type  (->gql-type (or (::gql-type entity)
                                    entity-id)
                                :out many? false)
         :many?    many?})
      ;;
      (osc/series? entity-id)
      (let [entity        (osc/series entity-id)
            series-entity (osc/series-entity entity)]
        ;; FIXME: How do we handle it if the they are returing `many`
        ;; `serieses`?
        (if (vector? return-type)
          (throw (ex-info "Sorry, not yet handling returning a list of `series`."
                          {:return-type return-type})))
        ;;
        {:entity-id entity-id
         :gql-type  (->gql-type (or (::gql-type      series-entity)
                                    (::osc/entity-id series-entity))
                                :out true false)
         :many?     true})
      ;;
      (native? entity-id)
      {:entity-id entity-id
       :gql-type  (->gql-type entity-id :out many? false)
       :many?     many?}
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
              (into {}))})

;;; --------------------------------------------------------------------------------

(defn entity->field-ref
  [entity-id in-out optional?]
  (cond
    (osc/scalar? entity-id)
    (let [entity (osc/scalar entity-id)]
      {:type (->gql-type (or (::gql-type      entity)
                             (::osc/entity-id entity))
                         nil false optional?)})
    ;;
    (osc/rec? entity-id)
    (let [entity (osc/rec entity-id)]
      {:type (->gql-type (or (::gql-type entity)
                             (::osc/entity-id entity))
                         in-out false optional?)})
    ;;
    (osc/series? entity-id)
    (let [entity        (osc/series entity-id)
          series-entity (osc/series-entity entity)]
      {:type (->gql-type (or (::gql-type      series-entity)
                             (::osc/entity-id series-entity))
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
                          (let [attr-entity (osc/attr-entity attr)]
                            [(clj-name->gql-name (or (::gql-id        attr)
                                                     (::osc/entity-id attr)))
                             (entity->field-ref attr-entity
                                                in-out
                                                (osc/optional? rec attr))])))
                   (into {}))}])

;;; --------------------------------------------------------------------------------

(defn scalar->enum
  [{::osc/keys [entity-id enums]
    ::keys [gql-id description] :as entity}]
  [(clj-name->gql-type-name (or gql-id entity-id) nil)
   (merge {:values (->> enums
                        (mapv (fn [enum]
                                (cond
                                  (keyword? enum)
                                  (csk/->SCREAMING_SNAKE_CASE_KEYWORD enum)
                                  ;;
                                  (map? enum)
                                  (let [{:keys [value description]} enum]
                                    (cond
                                      (and value description)
                                      {:enum-value  (csk/->SCREAMING_SNAKE_CASE_KEYWORD value)
                                       :description description}
                                      ;;
                                      value
                                      (csk/->SCREAMING_SNAKE_CASE_KEYWORD value)
                                      ;;
                                      :else
                                      (throw (ex-info "An enum must have a value."
                                                      {:entity entity}))))))))}
          (when description
            {:description description}))])

;;; --------------------------------------------------------------------------------

(defn schema->gql
  [schema]
  (let [ret-types (return-types schema)
        args      (end-point-args schema)
        ;;
        out-entity-ids (->> ret-types
                            (map second)
                            (map :entity-id)
                            distinct
                            osc/walk-entities)
        in-entity-ids  (->> args
                            (mapcat second)
                            (map :entity-id)
                            distinct
                            osc/walk-entities)
        ;;
        out-objects   (some->> out-entity-ids
                               (filter osc/rec?)
                               seq
                               (map #(-> % osc/rec (rec->object :out)))
                               (into {}))
        in-objects    (some->> in-entity-ids
                               (filter osc/rec?)
                               seq
                               (map #(-> % osc/rec (rec->object :in)))
                               (into {}))
        enums         (some->> (concat out-entity-ids in-entity-ids)
                               (filter osc/enum?)
                               seq
                               distinct
                               (map #(-> % osc/scalar scalar->enum))
                               (into {}))
        ;;
        ->endpoints   (fn [k]
                        (->> schema k
                             (map (fn [[clj-name end-point]]
                                    [(clj-name->gql-name clj-name)
                                     (hash-map* {:type (-> end-point :type ret-types :gql-type)}
                                                :args  (->> args
                                                            clj-name
                                                            (map (fn [{:keys [gql-arg-id gql-type]}]
                                                                   [gql-arg-id {:type gql-type}]))
                                                            (into {})
                                                            (nil-when->> empty?))
                                                :resolve (end-point :resolve))]))
                             (into {})))]
    {:enums         enums
     :objects       out-objects
     :input-objects in-objects
     :queries       (->endpoints :queries)
     :mutations     (->endpoints :mutations)}))
