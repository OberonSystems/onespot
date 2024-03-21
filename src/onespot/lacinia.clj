(ns onespot.lacinia
  (:require [clojure.string :as s]
            [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            ;;
            [oberon.utils :refer [nil-when->> map-entry hash-map*]]
            ;;
            [onespot.snakes :refer [->kebab-case-keyword ->PascalCaseKeyword ->camelCaseKeyword ->SCREAMING_SNAKE_CASE_KEYWORD
                                    keys->camel-case keys->kebab-case]]
            [onespot.cache :as cc]
            [onespot.core  :as os]
            [onespot.json  :as js]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp      (atom nil))
(defn-   ^:private set-tmp! [value] (swap! tmp (constantly value)))

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

(defn entity-id
  [entity-id]
  (-> entity-id os/canonical-entity-id os/pull ::entity-id))

(defn get-type
  [entity]
  (or (-> entity ::info :type)
      (:entity-id entity)))

(defn get-description
  [entity]
  (or (-> entity ::info :description)
      (:description entity)))

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

  {:people nil} <- :people is a os/series
  {:people {:optional? true} <- :people is a os/series

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
               (and info-type?
                    (-> info :type vector?)
                    (-> info :type first keyword?))
               {:type (-> info :type first) :many? true}
               ;;
               (and info-type?
                    (-> info :type keyword?))
               {:type (-> info :type)}
               ;;
               :else (ex-info (format "Can't convert arg `%s` to a field-ref." arg-id)
                              {:arg-id arg-id :info info}))
        ;;
        ;; When reading a schema, the type can actually be an entity.
        entity-id (:type info)
        many?     (or (-> info :many?)
                      (when (os/registered? entity-id)
                        (cond
                          (os/series? entity-id) true
                          (os/attr? entity-id)   (-> entity-id
                                                     os/attr
                                                     os/attr-entity
                                                     os/series?)
                          :else false)))]
    (cond
      (native? entity-id)
      {:entity-id  entity-id
       :clj-arg-id arg-id
       :gql-arg-id (->camelCaseKeyword arg-id)
       :gql-type   (->gql-type entity-id :in many? optional?)
       :many?      many?
       :optional?  optional?}
      ;;
      (os/scalar? entity-id)
      (let [entity (os/scalar entity-id)]
        {:entity-id  entity-id
         :clj-arg-id arg-id
         :gql-arg-id (->camelCaseKeyword arg-id)
         :gql-type   (->gql-type (get-type entity)
                                 :in many? optional?)
         :many?      many?
         :optional?  optional?})
      ;;
      (os/rec? entity-id)
      (let [entity (os/rec entity-id)]
        {:entity-id  entity-id
         :clj-arg-id arg-id
         :gql-arg-id (-> (or (when info-type? arg-id)
                             (entity ::entity-id)
                             arg-id)
                         ->camelCaseKeyword)
         :gql-type   (->gql-type (get-type entity)
                                 :in many? optional?)
         :many?      many?
         :optional?  optional?})
      ;;
      (os/series? entity-id)
      (let [entity        (os/series entity-id)
            series-entity (os/series-entity entity)]
        {:entity-id        entity-id
         :series-entity-id (:entity-id series-entity)
         ;;
         :clj-arg-id  arg-id
         :gql-arg-id  (-> (or (when info-type? arg-id)
                              (entity ::entity-id)
                              arg-id)
                          ->camelCaseKeyword)
         :gql-type    (->gql-type (get-type series-entity)
                                  :in many? optional?)
         :many?     many?
         :optional? optional?})
      ;;
      (os/attr? entity-id)
      (let [entity      (os/attr        entity-id)
            attr-entity (os/attr-entity entity)]
        {:entity-id      entity-id
         :attr-entity-id (:attr-entity-id entity)
         ;;
         :clj-arg-id  arg-id
         :gql-arg-id  (-> (or (when info-type? arg-id)
                              (entity ::entity-id)
                              arg-id)
                          ->camelCaseKeyword)
         :gql-type    (->gql-type (get-type attr-entity)
                                  :in many? optional?)})
      ;;
      :else (throw (ex-info (format "Can't convert arg-id `%s` to a field-ref." arg-id)
                            {:arg-id arg-id})))))

(defn compute-gql-args
  [schema]
  (->> (extract-map-entries schema (fn [[k v]]
                                     (and (map? v)
                                          (contains? v :args))))
       (map (fn [[k v]]
              [k
               (->> v
                    :args
                    (mapv (fn [[arg-id info]]
                            (arg->field-ref arg-id info))))]))
       (into {})))

;;; --------------------------------------------------------------------------------

(defn optional
  [entity-id]
  {:type entity-id
   :optional? true})

(defn canonicalise-return-type
  [return-type]
  (let [{:keys [entity-id many?] :as record}
        (cond
          (keyword? return-type)
          (if (and (os/registered? return-type)
                   (os/series?     return-type))
            {:entity-id (os/series-entity-id return-type)
             :many?     true
             :optional? false}
            {:entity-id return-type
             :many?     false
             :optional? false})
          ;;
          (and (vector? return-type)
               (-> return-type first keyword?))
          {:entity-id (first return-type)
           :many?     true
           :optional? false}
          ;;
          (and (map? return-type)
               (or (-> return-type :type keyword?)
                   (-> return-type :type vector?)))
          (let [{:keys [type optional?]} return-type]
            (assoc (canonicalise-return-type type)
                   :optional? (or optional? false)))
          ;;
          :else (throw (ex-info (format "Cannot canonicalise return-type: %s." return-type)
                                {:return-type return-type})))]
    (when (and (os/registered? entity-id)
               (os/series?     entity-id)
               many?)
      ;; FIXME: Decide if we should ever support this?
      (throw (ex-info "Sorry, not yet handling lists of `series` entities."
                      {:return-type return-type})))
    record))

(defn return-type->field-ref
  [return-type]
  (let [{:keys [entity-id many? optional?]} (canonicalise-return-type return-type)]
    (cond
      (native? entity-id)
      {:entity-id entity-id
       :gql-type  (->gql-type entity-id :out many? optional?)
       :many?     many?}
      ;;
      (os/scalar? entity-id)
      (let [entity (os/scalar entity-id)]
        {:entity-id entity-id
         :gql-type  (->gql-type (get-type entity)
                                :out many? optional?)
         :many?     many?})
      ;;
      (os/rec? entity-id)
      (let [entity (os/rec entity-id)]
        {:entity-id entity-id
         :gql-type  (->gql-type (get-type entity)
                                :out many? optional?)
         :many?    many?})
      ;;
      :else (throw (ex-info (format "Can't convert return-type `%s` to a field-ref." return-type)
                            {:return-type return-type})))))

(defn compute-gql-returns
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
    (os/scalar? entity-id)
    (let [entity (os/scalar entity-id)]
      {:type (->gql-type (get-type entity)
                         nil false optional?)})
    ;;
    (os/rec? entity-id)
    (let [entity (os/rec entity-id)]
      {:type (->gql-type (get-type entity)
                         in-out false optional?)})
    ;;
    (os/series? entity-id)
    (let [entity        (os/series entity-id)
          series-entity (os/series-entity entity)]
      {:type (->gql-type (get-type series-entity)
                         in-out true optional?)})
    ;;
    :else (throw (ex-info (format "Can't convert entity `%s` to a field-ref." entity-id)
                          {:entity-id entity-id}))))

(defn rec->object
  [{:keys [entity-id description] :as rec}
   in-out]
  [(clj-name->gql-type-name entity-id in-out)
   (hash-map* {:fields   (->> (os/rec-attrs rec :readonly? (= in-out :out))
                              (map (fn [attr]
                                     (let [attr-entity (os/attr-entity attr)]
                                       [(clj-name->gql-name (or (::entity-id attr)
                                                                (:entity-id  attr)))
                                        (entity->field-ref attr-entity
                                                           in-out
                                                           (os/optional? rec attr))])))
                              (into {}))}
              :description (or (get-description rec)
                               description))])

;;; --------------------------------------------------------------------------------

(defn scalar->enum
  [{:keys [enums]
    ::keys [entity-id description] :as entity}]
  [(clj-name->gql-type-name (or entity-id (:entity-id entity))
                            nil)
   (merge {:values (->> enums
                        (mapv (fn [enum]
                                (cond
                                  (keyword? enum)
                                  (->SCREAMING_SNAKE_CASE_KEYWORD enum)
                                  ;;
                                  (map? enum)
                                  (let [{:keys [value description]} enum]
                                    (cond
                                      (and value description)
                                      {:enum-value  (->SCREAMING_SNAKE_CASE_KEYWORD value)
                                       :description description}
                                      ;;
                                      value
                                      (->SCREAMING_SNAKE_CASE_KEYWORD value)
                                      ;;
                                      :else
                                      (throw (ex-info "An enum must have a value."
                                                      {:entity entity}))))))))}
          (when description
            {:description description}))])

;;; --------------------------------------------------------------------------------

(defn clj->endpoint
  [gql-args gql-returns [clj-name end-point]]
  ;;
  [(clj-name->gql-name clj-name)
   (hash-map* {:type (-> end-point :type gql-returns :gql-type)}
              :args  (->> gql-args
                          clj-name
                          (map (fn [{:keys [gql-arg-id gql-type]}]
                                 [gql-arg-id {:type gql-type}]))
                          (into {})
                          (nil-when->> empty?))
              :resolve (end-point :resolve))])

(defn schema->gql
  [schema]
  (let [gql-args    (compute-gql-args    schema)
        gql-returns (compute-gql-returns schema)
        ;;
        get-attr-ids   (fn [entity-id]
                         (os/rec-attr-ids entity-id :readonly? true))
        out-entity-ids (os/walk-entities (->> gql-returns
                                              (map second)
                                              (map :entity-id)
                                              (filter os/registered?)
                                              distinct)
                                         :readonly? true)
        in-entity-ids  (os/walk-entities (->> gql-args
                                              (mapcat second)
                                              (map :entity-id)
                                              (filter os/registered?)
                                              distinct))
        ;;
        out-objects   (some->> out-entity-ids
                               (filter os/rec?)
                               seq
                               (map #(-> % os/rec (rec->object :out)))
                               (into {}))
        in-objects    (some->> in-entity-ids
                               (filter os/rec?)
                               seq
                               (map #(-> % os/rec (rec->object :in)))
                               (into {}))
        enums         (some->> (concat out-entity-ids in-entity-ids)
                               (filter os/enum?)
                               seq
                               distinct
                               (map #(-> % os/scalar scalar->enum))
                               (into {}))
        ;;
        ->endpoints   (fn [endpoints]
                        (->> endpoints
                             (map #(clj->endpoint gql-args gql-returns %))
                             (into {})))]
    {:enums         enums
     :objects       out-objects
     :input-objects in-objects
     :queries       (some-> schema :queries   ->endpoints)
     :mutations     (some-> schema :mutations ->endpoints)}))

;;; --------------------------------------------------------------------------------

(defn resolvable?
  [x]
  (and (map-entry? x)
       (let [[k v] x]
         (and (map? v)
              (contains? v :resolve)))))

(defn wrap-resolvables
  "Postwalks the schema applying `wrap-resolvable` to all map-entries
  that have map `value` with a `resolve` key.

  Replacing the map `value` with the return value of `wrap-resolvable`."
  [schema wrap-resolvable]
  (postwalk (fn [v]
              (cond
                (resolvable? v)
                [(first v)
                 (wrap-resolvable (second v)
                                  :schema      schema
                                  :endpoint-id (first v))]
                ;;
                :else v))
            schema))

;;; --------------------------------------------------------------------------------

(defn ->lacinia-keys
  [m]
  (keys->camel-case m :rename-map (os/make-core-key->ns-key ::entity-id)))

(defn ->core-keys
  [m]
  (keys->kebab-case m :rename-map (os/make-ns-key->core-key ::entity-id)))

;;; --------------------------------------------------------------------------------

(defn make->lacinia-map
  [schema]
  (let [args    (->> (compute-gql-args schema)
                     (map (fn [[op-key arg-defs]]
                            [op-key
                             (->> arg-defs
                                  (map (juxt :clj-arg-id :entity-id))
                                  (into {}))]))
                     (into {}))
        returns (->> (extract-map-entries schema (fn [[k v]]
                                                   (and (map? v)
                                                        (contains? v :type))))
                     (map (fn [[k v]]
                            [k (-> v
                                   :type
                                   return-type->field-ref
                                   (select-keys [:entity-id :many? :optional?]))]))
                     (into {}))]
    (->> (concat (keys args) (keys returns))
         distinct
         (map (fn [k]
                [k {:arg-types   (k args)
                    :return-type (k returns)}]))
         (into {}))))

(defn args->core
  "Converts Lacinia args to core values."
  [record schema endpoint-id]
  (let [entity-id-lookup (or (-> (cc/pull ::->lacinia-map #(make->lacinia-map schema))
                                 (get endpoint-id)
                                 :arg-types)
                             {})]
    (->> record
         (map (fn [[k v]]
                [k (js/->core (entity-id-lookup k k) v)]))
         (into {}))))

(defn core->lacinia
  [value schema endpoint-id]
  (let [{:keys [entity-id many? optional?]} (-> (cc/pull ::->lacinia-map #(make->lacinia-map schema))
                                                (get endpoint-id)
                                                :return-type)
        ->lacinia (fn [v]
                    (-> (js/->json entity-id v)
                        ->lacinia-keys))]
    (if many?
      (or (some->> value (mapv ->lacinia))
          (if optional? nil []))
      (->lacinia value))))

;;;

(defn resolved-value?
  [value]
  (and (map? value)
       (contains? value :resolved-value)))

(defn wrap-convert-gql
  [{:keys [resolve] :as record} & {:keys [schema endpoint-id debug?]}]
  (assoc record
         :resolve (fn [context args value]
                    (when debug?
                      (println 'wrap-convert-gql-args endpoint-id)
                      (pprint args))
                    (let [args   (-> args
                                     ->core-keys
                                     (args->core schema endpoint-id))
                          _      (when debug? (pprint args))
                          result (resolve context args value)]
                      (when debug? (pprint result))
                      (if (resolved-value? result)
                        result
                        (core->lacinia result schema endpoint-id))))))
