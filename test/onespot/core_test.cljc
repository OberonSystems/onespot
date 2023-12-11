(ns onespot.core-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.validators :refer :all]
            [onespot.validate :refer [validate]]
            [onespot.core :refer :all :as osc]
            :reload))

(defn register-scalars!
  []
  (clear!)
  (scalar! ::string1 non-blank-string?)
  (scalar! ::string2 non-blank-string?
           ::osc/label       "My Label"
           ::osc/description "My Description")
  (scalar! ::record-id positive-integer?))

(defn register-attrs!
  []
  (register-scalars!)
  (attr! ::person-id  ::record-id)
  (attr! ::given-name ::string1)
  (attr! ::family-name ::string1
         ::os/label "The Family Name"))

(deftest test-scalars-1
  (register-common!)
  (is (= (label ::string1) "String 1"))
  (is (= (validator ::string1) non-blank-string?))

  (is (= (label       ::string2) "My Label"))
  (is (= (description ::string2) "My Description"))

  (is (= (validate ::string1 nil)        [{:path [] :feedback {:code :missing}}]))
  (is (= (validate ::string1 "")         [{:path [], :feedback {:code    :bad-value
                                                                :message "Must be a non blank string."}}]))
  (is (= (validate ::string1 "a string") nil)))

(deftest test-attrs-1
  (register-attrs!)

  (is (= (label ::given-name) "Given Name"))
  (is (= (label ::family-name) "The Family Name"))
  ;;
  (validate ::given-name {::given-name "this is here"}))

(deftest test-recs-1
  (register-attrs!)
  (rec! ::person
        [::person-id ::given-name ::family-name]
        ::osc/identity-ks [::person-id]
        ;;
        ::something-else :hi-there)

  (rec! ::person-token
        (osc/identity-keys ::person))

  )
