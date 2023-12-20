(ns onespot.common
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.lacinia :as osl]
            :reload))

(defn register-common!
  []
  (clear!)
  (scalar! :integer non-blank-string)
  (scalar! :string  non-blank-string)
  (scalar! :boolean true-or-false)

  ;; (scalar! :string2 global-keyword ::osj/kind :enum)

  (attr! :person-id   :integer)
  (attr! :given-name  :string)
  (attr! :family-name :string)
  (attr! :active?     :boolean
         ::osl/gql-id :is-active)

  (rec! :person
        [:person-id
         :given-name
         :family-name
         :active?]
        ::osc/identity-ids [:person-id])

  (rec! :new-person
        (osc/rec-value-ids :person)))
