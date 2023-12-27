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

(deftest test-return-types
  (register-common!)

  ()

  (let [schema {:queries   {:fetch-people  {:type    [:person]
                                            :resolve fetch-people}
                            :fetch-person  {:type    :person
                                            :args    {:person-id nil}
                                            :resolve fetch-person}}
                :mutations {:add-person    {:type    :person
                                            :args    {:new-person nil}
                                            :resolve dummy-fn}
                            :modify-person {:type    :person
                                            :args    {:person nil}
                                            :resolve dummy-fn}}}]
    (is (= (->> (osl/end-point-types schema)
                (map second)
                (map (juxt :type-key :type-spec)))
           [[[:person] '(not-null (list (not-null :person-out)))]
            [:person   '(not-null :person-out)]]))

    (is (= (osl/end-point-args schema)
           [:new-person :person :person-id]))

    ))

(defn return-type=
  [a b]
  ;; For some reason doing an `=` on the validator functions always
  ;; returns false, so doing the type to str conversion is the best
  ;; I've come up with for testing and is probably fine for our
  ;; purposes.
  (let [validator->comparable (update-in x
                                         [:entity :onespot.core/validator]
                                         #(-> % type str))]
    (= (validator->comparable a)
       (validator->comparable b))))

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

(defn arg-type=
  [a b]
  ;; For some reason doing an `=` on the validator functions always
  ;; returns false, so doing the type to str conversion is the best
  ;; I've come up with for testing and is probably fine for our
  ;; purposes.
  (let [validator->comparable (update-in x
                                         [:attr-type :onespot.core/validator]
                                         #(-> % type str))]
    (= (validator->comparable a)
       (validator->comparable b))))

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
