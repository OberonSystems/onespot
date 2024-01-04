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
  (register-common!)

  (is (= (osl/return-type->field-ref :string)
         {:entity-id :string
          :gql-type  '(not-null String)
          :many?     false}))

  (is (= (osl/return-type->field-ref [:string])
         {:entity-id :string
          :gql-type  '(not-null (list (not-null String)))
          :many?     true}))

  (is (= (osl/return-type->field-ref :person)
         {:entity-id :person
          :gql-type  '(not-null :PersonOut)
          :many?     false}))

  (is (= (osl/return-type->field-ref :people)
         {:entity-id :people
          :gql-type '(not-null (list (not-null :PersonOut)))
          :many? true})))

(deftest test-arg-types
  (register-common!)
  (is (= (osl/arg->field-ref :person-id nil)
         {:entity-id      :person-id
          :attr-entity-id :positive-integer
          :clj-arg-id     :person-id
          :gql-arg-id     :personId
          :gql-type       '(not-null Int)}))

  (is (= (osl/arg->field-ref :person nil)
         {:entity-id  :person
          :clj-arg-id :person
          :gql-arg-id :person
          :gql-type   '(not-null :PersonIn)
          :many?      false
          :optional?  false})))

(deftest test-entity->field-ref
  (register-common!)
  (is (= (osl/entity->field-ref :string :in false)
         {:type '(not-null String)}))

  (is (= (osl/entity->field-ref :string :in true)
         {:type 'String}))

  (is (= (osl/entity->field-ref :person :in false)
         {:type '(not-null :PersonIn)}))

  (is (= (osl/entity->field-ref :people :in false)
         {:type '(not-null (list (not-null :PersonIn)))}))

  (is (= (osl/entity->field-ref :people :out true)
         {:type '(list (not-null :PersonOut))})))

(deftest test-rec->gql-object
  (register-common!)
  (is (= (osl/rec->object (osc/pull :person) :in)
         '[:PersonIn
           {:fields
            {:personId   {:type (not-null Int)}
             :givenName  {:type (not-null String)}
             :familyName {:type (not-null String)}
             :shirtSize  {:type (not-null :ShirtSizeType)}
             :isActive   {:type (not-null Boolean)}}}])))

(deftest test-simple-schema
  (register-common!)
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
                             :gql-type       '(not-null Int)}]
            :modify-person [{:entity-id  :person
                             :clj-arg-id :person
                             :gql-arg-id :person
                             :gql-type   '(not-null :PersonIn)
                             :many?      false
                             :optional?  false}]}))
    ;;
    (let [{:keys [enums objects input-objects queries mutations] :as gql} (osl/schema->gql schema)]
      (is (= (keys enums)         [:ShirtSizeType]))
      (is (= (keys objects)       [:PersonOut]))
      (is (= (keys input-objects) [:PersonIn]))
      (is (= queries
             '{:fetchPerson {:type    (not-null :PersonOut)
                             :args    {:personId {:type (not-null Int)}}
                             :resolve :resolver-placeholder}
               :fetchPeople {:type (not-null (list (not-null :PersonOut)))}}))
      (is (= mutations
             '{:modifyPerson {:type (not-null :PersonOut)
                              :args {:person {:type (not-null :PersonIn)}}}})))))
