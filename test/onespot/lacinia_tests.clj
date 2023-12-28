(ns onespot.lacinia-tests
  (:require [clojure.test :refer [deftest testing is run-tests]]
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

(defn validator->comparable
  [x path]
  ;; For some reason doing an `=` on the validator functions always
  ;; returns false, so doing the type to str conversion is the best
  ;; I've come up with for testing and is probably fine for our
  ;; purposes.
  (update-in x
             path ;; [:entity :onespot.core/validator]
             (comp str type)))

(defn return-type=
  [a b]
  (= (validator->comparable a [:entity :onespot.core/validator])
     (validator->comparable b [:entity :onespot.core/validator])))

(defn arg-type=
  [a b]
  (= (validator->comparable a [:attr-type :onespot.core/validator])
     (validator->comparable b [:attr-type :onespot.core/validator])))

;;; --------------------------------------------------------------------------------

(deftest test-return-types
  (register-common!)

  (is (return-type= (osl/return-type->field-ref :string)
                    {:entity #:onespot.core{:entity-id :string,
                                            :validator onespot.validators/non-blank-string,
                                            :kind :onespot.core/scalar},
                     :gql-type '(not-null String),
                     :many? false}))

  (is (return-type= (osl/return-type->field-ref [:string])
                    {:entity
                     #:onespot.core{:entity-id :string,
                                    :validator onespot.validators/non-blank-string,
                                    :kind :onespot.core/scalar},
                     :gql-type '(not-null (list (not-null String))),
                     :many? true}))

  (is (return-type= (osl/return-type->field-ref :person)
                    {:entity #:onespot.core{:attr-ids
                                            [:person-id :given-name :family-name :active?],
                                            :entity-id :person,
                                            :identity-ids [:person-id],
                                            :kind :onespot.core/rec,
                                            :optional-set nil,
                                            :value-ids [:given-name :family-name :active?]},
                     :gql-type '(not-null :PersonOut),
                     :many? false}))

  (is (= (osl/return-type->field-ref :people)
         {:entity
          #:onespot.core{:entity-id :people,
                         :type-id :person,
                         :kind :onespot.core/series},
          :gql-type '(not-null (list (not-null :PersonOut))),
          :many? true})))

(deftest test-arg-types
  (register-common!)
  (is (arg-type= (osl/arg->field-ref :person-id nil)
                 {:entity
                  #:onespot.core{:entity-id :person-id,
                                 :type-id :int,
                                 :kind :onespot.core/attr},
                  :attr-type {:onespot.lacinia/gql-type :int,
                              :onespot.core/validator onespot.validators/positive-integer
                              :onespot.core/entity-id :int,
                              :onespot.core/kind :onespot.core/scalar},
                  :clj-arg-id :person-id,
                  :gql-arg-id :personId,
                  :gql-type '(not-null Int)}))

  (is (arg-type= (osl/arg->field-ref :person nil)
                 {:entity #:onespot.core{:attr-ids
                                         [:person-id :given-name :family-name :active?],
                                         :entity-id :person,
                                         :identity-ids [:person-id],
                                         :kind :onespot.core/rec,
                                         :optional-set nil,
                                         :value-ids [:given-name :family-name :active?]},
                  :clj-arg-id :person,
                  :gql-arg-id :person,
                  :gql-type '(not-null :PersonIn),
                  :many? false,
                  :optional? false})))

(deftest test-entity->field-ref
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

  (is (= (osl/rec->object (osc/pull :person) :in)
         '[:PersonIn
           {:fields
            {:personId   {:type (not-null Int)},
             :givenName  {:type (not-null String)},
             :familyName {:type (not-null String)},
             :isActive   {:type (not-null Boolean)}}}])))
