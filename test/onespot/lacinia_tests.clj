(ns onespot.lacinia-tests
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing is run-tests]]
            [clojure.data :refer [diff]])
  (:require [onespot.core :refer :all :as os]
            [onespot.json :as js]
            [onespot.lacinia :as lc]
            ;;
            [onespot.common :refer :all]
            :reload)
  (:import [java.time LocalDate]))

;;; --------------------------------------------------------------------------------

(deftest test-return-types
  (register-all!)

  ;; Native GQL String types
  (is (= (lc/return-type->field-ref :string)
         {:entity-id :string
          :gql-type  '(non-null String)
          :many?     false}))

  (is (= (lc/return-type->field-ref (lc/optional :string))
         {:entity-id :string
          :gql-type  'String
          :many?     false}))

  (is (= (lc/return-type->field-ref [:string])
         {:entity-id :string
          :gql-type  '(non-null (list (non-null String)))
          :many?     true}))

  (is (= (lc/return-type->field-ref (lc/optional [:string]))
         {:entity-id :string
          :gql-type  '(list (non-null String))
          :many?     true}))

  (is (= (lc/return-type->field-ref :person)
         {:entity-id :person
          :gql-type  '(non-null :PersonOut)
          :many?     false}))

  (is (= (lc/return-type->field-ref :people)
         {:entity-id :person
          :gql-type '(non-null (list (non-null :PersonOut)))
          :many? true})))

(deftest test-arg-types
  (register-all!)
  (is (= (lc/arg->field-ref :person-id nil)
         {:entity-id      :person-id
          :attr-entity-id ::os/positive-integer
          :clj-arg-id     :person-id
          :gql-arg-id     :personId
          :gql-type       '(non-null Int)}))

  (is (= (lc/arg->field-ref :person nil)
         {:entity-id  :person
          :clj-arg-id :person
          :gql-arg-id :person
          :gql-type   '(non-null :PersonIn)
          :many?      false
          :optional?  false})))

(deftest test-entity->field-ref
  (register-all!)
  (is (= (lc/entity->field-ref ::os/string :in false)
         {:type '(non-null String)}))

  (is (= (lc/entity->field-ref ::os/string :in true)
         {:type 'String}))

  (is (= (lc/entity->field-ref :person :in false)
         {:type '(non-null :PersonIn)}))

  (is (= (lc/entity->field-ref :people :in false)
         {:type '(non-null (list (non-null :PersonIn)))}))

  (is (= (lc/entity->field-ref :people :out true)
         {:type '(list (non-null :PersonOut))})))

(deftest test-rec->gql-object
  (register-all!)
  (is (= (lc/rec->object (os/rec :person) :in)
         '[:PersonIn
           {:fields
            {:personId   {:type (non-null Int)}
             :givenName  {:type (non-null String)}
             :familyName {:type (non-null String)}
             :shirtSize  {:type (non-null :ShirtSizeType)}
             :dob        {:type String}
             :isActive   {:type (non-null Boolean)}}}]))

  (is (= (lc/rec->object (os/rec :person-with-readonly) :in)
         '[:PersonWithReadonlyIn
           {:fields
            {:personId {:type (non-null Int)}
             :givenName {:type (non-null String)}}}]))

  (is (= (lc/rec->object (os/rec :person-with-readonly) :out)
         '[:PersonWithReadonlyOut
           {:fields
            {:personId   {:type (non-null Int)}
             :givenName  {:type (non-null String)}
             :familyName {:type (non-null String)}}}]))

  (is (= (lc/rec->object (os/rec :person-with-optional-fields) :out)
         '[:PersonWithOptionalFieldsOut
           {:fields
            {:personId   {:type (non-null Int)}
             :givenName  {:type String}
             :familyName {:type (non-null String)}}}]))

  (is (= (lc/rec->object (os/rec :person-with-core-description) :out)
         '[:PersonWithCoreDescriptionOut
           {:fields {:personId  {:type (non-null Int)}
                     :givenName {:type (non-null String)}}
            :description "Core Description"}]))

  (is (= (lc/rec->object (os/rec :person-with-lacinia-description) :out)
         '[:PersonWithLaciniaDescriptionOut
           {:fields {:personId  {:type (non-null Int)}
                     :givenName {:type (non-null String)}}
            :description "Lacinia Description"}])))

(deftest test-enums
  (register-all!)
  (is (= (-> :shirt-size-type os/scalar lc/scalar->enum)
         [:ShirtSizeType
          {:values
           [{:enum-value :SM :description "Small"}
            {:enum-value :MD :description "Medium"}
            {:enum-value :LG :description "Large"}
            {:enum-value :XL :description "Extra Large"}]}]))

  (scalar! :enum-1 (fn [& _])
           :enums (os/canonicalise-enums [[:value1 :description]
                                          :value2]))
  (is (= (-> :enum-1 os/scalar lc/scalar->enum)
         [:Enum1 {:values [{:enum-value :VALUE_1, :description :description} :VALUE_2]}])))

(deftest test-simple-schema
  (register-all!)
  (let [schema {:queries {:fetch-person {:type    :person
                                         :args    {:person-id nil}
                                         :resolve :resolver-placeholder}
                          :fetch-people {:type :people}}
                ;;
                :mutations {:modify-person {:type :person
                                            :args {:person nil}}}}]
    (is (= (keys (lc/compute-gql-returns schema))
           [:person :people]))

    (is (= (lc/compute-gql-args schema)
           {:fetch-person  [{:entity-id      :person-id
                             :attr-entity-id ::os/positive-integer
                             :clj-arg-id     :person-id
                             :gql-arg-id     :personId
                             :gql-type       '(non-null Int)}]
            :modify-person [{:entity-id  :person
                             :clj-arg-id :person
                             :gql-arg-id :person
                             :gql-type   '(non-null :PersonIn)
                             :many?      false
                             :optional?  false}]}))
    ;;
    (let [{:keys [enums objects input-objects queries mutations] :as gql} (lc/schema->gql schema)]
      (is (= (keys enums)         [:ShirtSizeType]))
      (is (= (keys objects)       [:PersonOut]))
      (is (= (keys input-objects) [:PersonIn]))
      (is (= queries
             '{:fetchPerson {:type    (non-null :PersonOut)
                             :args    {:personId {:type (non-null Int)}}
                             :resolve :resolver-placeholder}
               :fetchPeople {:type (non-null (list (non-null :PersonOut)))}}))
      (is (= mutations
             '{:modifyPerson {:type (non-null :PersonOut)
                              :args {:person {:type (non-null :PersonIn)}}}})))))

(deftest test-parsing-gql-args-01
  (register-all!)

  ;; Tests native and attributes used as positional args.
  (let [schema {:queries {:query-1 {:type :string
                                    :args {:native-int    {:type :int    :optional? true}
                                           :native-string {:type :string :optional? false}
                                           ;;
                                           :given-name  nil
                                           :family-name nil}}}}
        gql-schema (lc/schema->gql schema)
        ;;
        q1 (get-in gql-schema [:queries :query1 :args])
        a1 {:nativeInt    3
            :nativeString "test"
            :givenName    "given-name"
            :familyName   "family-name"}]
    ;;
    (is (= q1
           '{:nativeInt    {:type Int}
             :nativeString {:type (non-null String)}
             :givenName    {:type (non-null String)}
             :familyName   {:type (non-null String)}}))

    (is (= (lc/->core-keys a1)
           {:native-int    3
            :native-string "test"
            :given-name    "given-name"
            :family-name   "family-name"}))

    (is (= (->> (lc/->core-keys a1)
                js/->core)
           {:native-int    3
            :native-string "test"
            :given-name    "given-name"
            :family-name   "family-name"}))))

(deftest test-parsing-gql-args-02
  (register-all!)

  (let [schema {:queries {:query-1 {:type :boolean
                                    :args {:person nil}}}}
        gql-schema (lc/schema->gql schema)
        ;;
        q1 (get-in gql-schema [:queries :query1 :args])
        a1 {:person {:personId   3
                     :givenName  "given-name"
                     :familyName "family-name"
                     :shirtSize  "SM"
                     :dob        "2022-01-01"
                     :isActive   true}}]
    (is (= q1
           '{:person {:type (non-null :PersonIn)}}))

    (is (= (lc/->core-keys a1)
           {:person {:person-id   3
                     :given-name  "given-name"
                     :family-name "family-name"
                     :shirt-size  "SM"
                     :dob         "2022-01-01"
                     :active?     true}}))

    (is (= (->> (lc/->core-keys a1)
                js/->core)
           {:person {:person-id   3
                     :given-name  "given-name"
                     :family-name "family-name"
                     :shirt-size  :sm
                     :dob         (LocalDate/parse "2022-01-01")
                     :active?     true}}))

    (is (= (->> {:person {:person-id   3
                          :given-name  "given-name"
                          :family-name "family-name"
                          :shirt-size  :sm
                          :dob         (LocalDate/parse "2022-01-01")
                          :active?     true}}
                js/->json
                lc/->lacinia-keys)
           {:person {:personId   3
                     :givenName  "given-name"
                     :familyName "family-name"
                     :shirtSize  "SM"
                     :dob        "2022-01-01"
                     :isActive   true}}))))

(deftest test-parsing-gql-args-03
  (register-all!)
  (let [schema {:queries {:query-1 {:type :boolean
                                    :args {:bob-the-person :person}}}}
        gql-schema (lc/schema->gql schema)
        arg-map    (lc/schema->arg-entity-map schema)
        ;;
        q1 (get-in gql-schema [:queries :query1 :args])]
    (is (= q1 '{:bobThePerson {:type (non-null :PersonIn)}}))
    (is (= arg-map {:query-1 {:bob-the-person :person}}))

    (is (= (-> {:bob-the-person {:personId   3
                                 :givenName  "given-name"
                                 :familyName "family-name"
                                 :shirtSize  "SM"
                                 :dob        "2022-01-01"
                                 :isActive   true}}
               lc/->core-keys
               (lc/args->core schema :query-1))
           {:bob-the-person {:person-id   3
                             :given-name  "given-name"
                             :family-name "family-name"
                             :shirt-size  :sm
                             :dob         (LocalDate/parse "2022-01-01")
                             :active?     true}}))))
