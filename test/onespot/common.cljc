(ns onespot.common
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.lacinia :as osl]
            :reload))

(def +shirt-sizes+ #{:sm :md :lg :xl})

(defn validate-shirt-size
  [x]
  (when-not (+shirt-sizes+ x)
    {:code    :bad-value
     :message "Must be a shirt size."
     :value   x}))

(defn register-common!
  []
  (clear!)
  (scalar! :positive-integer positive-integer ::osl/gql-type :int)
  (scalar! :string           non-blank-string)
  (scalar! :boolean          true-or-false)
  (scalar! :shirt-size-type  validate-shirt-size
           ;;
           :osl/gql-type  :enum
           :osl/gql-enums +shirt-sizes+
           ;;
           :pg/pg-type :enum)

  (attr! :person-id   :positive-integer)
  (attr! :given-name  :string)
  (attr! :family-name :string)
  (attr! :shirt-size  :shirt-size-type)
  (attr! :active?     :boolean
         ::osl/gql-id :is-active)

  (rec! :person
        [:person-id
         :given-name
         :family-name
         :shirt-size
         :active?]
        ::osc/identity-ids [:person-id])

  (rec! :new-person
        (osc/rec-value-ids :person))

  (series! :people :person))
