(ns onespot.lacinia-tests
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing is run-tests]]
            [clojure.data :refer [diff]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.lacinia :as osl]
            ;;
            [onespot.common :refer :all]
            :reload))

(defn fetch-people
  [])

(defn fetch-person
  [])

(defn dummy-fn
  [])

;;; --------------------------------------------------------------------------------

(deftest test-return-types
  (register-all!)

  (is (= (osl/return-type->field-ref :string)
         {:entity-id :string
          :gql-type  '(non-null String)
          :many?     false}))

  (is (= (osl/return-type->field-ref [:string])
         {:entity-id :string
          :gql-type  '(non-null (list (non-null String)))
          :many?     true}))

  (is (= (osl/return-type->field-ref :person)
         {:entity-id :person
          :gql-type  '(non-null :PersonOut)
          :many?     false}))

  (is (= (osl/return-type->field-ref :people)
         {:entity-id :people
          :gql-type '(non-null (list (non-null :PersonOut)))
          :many? true})))

(deftest test-arg-types
  (register-all!)
  (is (= (osl/arg->field-ref :person-id nil)
         {:entity-id      :person-id
          :attr-entity-id :positive-integer
          :clj-arg-id     :person-id
          :gql-arg-id     :personId
          :gql-type       '(non-null Int)}))

  (is (= (osl/arg->field-ref :person nil)
         {:entity-id  :person
          :clj-arg-id :person
          :gql-arg-id :person
          :gql-type   '(non-null :PersonIn)
          :many?      false
          :optional?  false})))

(deftest test-entity->field-ref
  (register-all!)
  (is (= (osl/entity->field-ref :string :in false)
         {:type '(non-null String)}))

  (is (= (osl/entity->field-ref :string :in true)
         {:type 'String}))

  (is (= (osl/entity->field-ref :person :in false)
         {:type '(non-null :PersonIn)}))

  (is (= (osl/entity->field-ref :people :in false)
         {:type '(non-null (list (non-null :PersonIn)))}))

  (is (= (osl/entity->field-ref :people :out true)
         {:type '(list (non-null :PersonOut))})))

(deftest test-rec->gql-object
  (register-all!)
  (is (= (osl/rec->object (osc/pull :person) :in)
         '[:PersonIn
           {:fields
            {:personId   {:type (non-null Int)}
             :givenName  {:type (non-null String)}
             :familyName {:type (non-null String)}
             :shirtSize  {:type (non-null :ShirtSizeType)}
             :isActive   {:type (non-null Boolean)}}}])))

(deftest test-enums
  (register-all!)
  (is (= (-> :shirt-size-type osc/scalar osl/scalar->enum)
         [:ShirtSizeType
          {:values
           [{:enum-value :SM :description "Small"}
            {:enum-value :MD :description "Medium"}
            {:enum-value :LG :description "Large"}
            {:enum-value :XL :description "Extra Large"}]}]))

  (scalar! :enum-1     (fn [& _])
           ::osc/enums (osc/canonicalise-enums [[:value1 :description]
                                                :value2]))
  (is (= (-> :enum-1 osc/scalar osl/scalar->enum)
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
    (is (= (keys (osl/return-types schema))
           [:person :people]))

    (is (= (osl/end-point-args schema)
           {:fetch-person  [{:entity-id      :person-id
                             :attr-entity-id :positive-integer
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
    (let [{:keys [enums objects input-objects queries mutations] :as gql} (osl/schema->gql schema)]
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
