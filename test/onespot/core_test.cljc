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
         ::osc/label "The Family Name"))

(deftest test-scalars-1
  (register-scalars!)
  (is (= (label ::string1) "String 1"))
  (is (= (validator ::string1) non-blank-string?))

  (is (= (label       ::string2) "My Label"))
  (is (= (description ::string2) "My Description"))

  (is (= (validate ::string1 nil)        [{:path []
                                           :feedback {:code :missing
                                                      :message "Value cannot be nil for a required entity."
                                                      :entity-id :onespot.core-test/string1
                                                      :value nil}}]))
  (is (= (validate ::string1 "")         [{:path [], :feedback {:code    :bad-value
                                                                :message "Must be a non blank string."}}]))
  (is (= (validate ::string1 "a string") nil)))

(deftest test-attrs-1
  (register-attrs!)

  (is (= (label ::given-name) "Given Name"))
  (is (= (label ::family-name) "The Family Name"))
  ;;
  (is (nil? (validate ::given-name {::given-name "a given name"})))
  ;;
  (is (= (validate ::given-name {::given-name nil})
         [{:path     [:onespot.core-test/given-name]
           :feedback {:code :missing
                      :message "Value cannot be nil for a required entity."
                      :entity-id :onespot.core-test/given-name
                      :value nil}}]))
  (is (= (validate ::given-name {})
         [{:path [:onespot.core-test/given-name]
           :feedback {:code      :missing
                      :message   "Attr: `:onespot.core-test/given-name` cannot find itself in hashmap value."
                      :entity-id :onespot.core-test/given-name
                      :value     {}}}])))

(deftest test-recs-1
  (register-attrs!)
  (rec! ::person
        [::person-id ::given-name ::family-name]
        ::osc/identity-ks [::person-id]
        ;;
        ::something-else :hi-there)

  (rec! ::person-token
        (osc/identity-keys ::person))

  (validate ::person {::given-name ""})
  (is (= (validate ::person {::given-name ""})
         [{:path     [:onespot.core-test/person :onespot.core-test/person-id]
           :feedback {:code :missing
                      :message
                      "Attr: `:onespot.core-test/person-id` cannot find itself in hashmap value."
                      :entity-id :onespot.core-test/person-id
                      :value #:onespot.core-test{:given-name ""}}}
          {:path     [:onespot.core-test/person :onespot.core-test/given-name]
           :feedback {:code :bad-value :message "Must be a non blank string."}}
          {:path     [:onespot.core-test/person :onespot.core-test/family-name]
           :feedback {:code :missing
                      :message
                      "Attr: `:onespot.core-test/family-name` cannot find itself in hashmap value."
                      :entity-id :onespot.core-test/family-name
                      :value #:onespot.core-test{:given-name ""}}}])))
