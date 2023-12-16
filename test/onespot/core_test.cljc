(ns onespot.core-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.validators :refer :all]
            [onespot.validate :refer [validate]]
            [onespot.core :refer :all :as osc]
            :reload))

(defn code?
  ([validation path code]
   (and (= path (-> validation first :path))
        (= code (-> validation first :feedback :code))))
  ([validation code]
   (= (-> validation
          first
          :feedback
          :code)
      code)))

(defn register-scalars!
  []
  (clear!)
  (scalar! :string1 non-blank-string?)
  (scalar! :string2 non-blank-string?
           ::osc/label       "My Label"
           ::osc/description "My Description")
  (scalar! :record-id positive-integer?)
  ;;
  (scalar! :contact-type-enum #(one-of % #{:mobile :email})))

(defn register-attrs!
  []
  (register-scalars!)
  (attr! :person-id   :record-id)
  (attr! :given-name  :string1)
  (attr! :nickname    :string1)
  (attr! :family-name :string1
         ::osc/label "The Family Name")
  ;;
  (attr! :contact-type  :contact-type-enum)
  (attr! :contact-value :string1))

(deftest test-scalars-1
  (register-scalars!)
  (is (= (label     :string1) "String 1"))
  (is (= (validator :string1) non-blank-string?))

  (is (= (label       :string2) "My Label"))
  (is (= (description :string2) "My Description"))

  (is (code? (validate :string1 nil) :missing-value))
  (is (code? (validate :string1 "")  :bad-value))
  (is (nil?  (validate :string1 "a string"))))

(deftest test-attrs-1
  (register-attrs!)

  (is (= (label :given-name) "Given Name"))
  (is (= (label :family-name) "The Family Name"))
  ;;
  (is (code? (validate :given-name nil)               :missing-value))
  (is (code? (validate :given-name {})                :missing-value))
  (is (code? (validate :given-name {:given-name nil}) :missing-value))
  (is (nil?  (validate :given-name {:given-name "a given name"}))))

(deftest test-recs-1
  (register-attrs!)
  (rec! :person
        [:person-id :given-name :nickname :family-name]

        ::osc/identity-ids [:person-id]
        ::osc/optional-ids [:nickname]
        ;;
        ::something-else :hi-there)

  (is (= (osc/rec-attr-ids :person)
         [:person-id :given-name :nickname :family-name]))

  (is (= (osc/rec-optional-set :person)
         #{:nickname}))

  (is (= (osc/rec-content :person {:person-id "my-id" :given-name "given" :family-name "family"
                                   :other-stuff :that :gets :ignored})
         {:person-id "my-id" :given-name "given", :family-name "family"}))

  (is (= (osc/rec-identity :person {:person-id "my-id" :given-name "given" :family-name "family"})
         {:person-id "my-id"}))

  (is (= (osc/rec-values :person {:person-id "my-id" :given-name "given" :family-name "family"})
         {:given-name "given" :family-name "family"}))

  (is (code? (validate :person {:person-id   "my-id"
                                :given-name  "g"
                                :nickname    "n"
                                :family-name "f"})
             :bad-value))
  (is (code? (validate :person {:person-id   10
                                :given-name  nil
                                :nickname    "n"
                                :family-name "f"})
             :missing-value))
  (is (code? (validate :person {:given-name  nil
                                :nickname    "n"
                                :family-name "f"})
             :missing-attr))
  (is (nil? (validate :person {:person-id   10
                               :given-name  "g"
                               :nickname    "n"
                               :family-name "f"}))))

(deftest test-recs-nested-1
  (register-attrs!)
  (rec!  :contact-info-type [:contact-type :contact-value])
  (attr! :contact-info :contact-info-type)

  (rec! :person
        [:person-id :given-name :family-name :contact-info]
        ::osc/identity-ids [:person-id])

  (is (code? (validate :person {:person-id 1234
                                :given-name "gn"
                                :family-name "fn"
                                :contact-info {:contact-type :mobilecc
                                               :contact-value "0123 123 123"}})
             :bad-value))

  (is (code? (validate :person {:person-id 1234
                                :given-name "gn"
                                :family-name "fn"
                                :contact-info nil})
             [:contact-info]
             :missing-value))

  (is (nil? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info {:contact-type :mobile
                                              :contact-value "0123 123 123"}}))))

(deftest test-recs-nested-2
  (register-attrs!)
  (rec!  :contact-info-type [:contact-type :contact-value])
  (attr! :contact-info :contact-info-type)

  (rec! :person
        [:person-id :given-name :family-name :contact-info]
        ::osc/identity-ids [:person-id]
        ::osc/optional-ids [:contact-info]
        ::osc/validator    (fn [{:keys [family-name]}]
                             (when-not (= family-name "fn")
                               {:code :bad-value
                                :message "Family Name must be 'fn'"
                                :value family-name})))

  (is (code? (validate :person {:person-id 1234
                                :given-name "gn"
                                :family-name "fn"
                                :contact-info {:contact-type :mobilecc
                                               :contact-value "0123 123 123"}})
             :bad-value))

  (is (nil? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info {:contact-type :mobile
                                              :contact-value "0123 123 123"}})))

  (is (nil? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info nil})))

  (is (code? (validate :person {:person-id 1234
                                :given-name "gn"
                                :family-name "FN-BAD"
                                :contact-info nil})
             :bad-value)))

(deftest test-series-1
  (register-attrs!)
  (series! :some-strings :string1)

  (is (code? (validate :some-strings nil)
             :missing-value))

  (is (code? (validate :some-strings [])
             :empty-value))

  (is (code? (validate :some-strings ["test" :this])
             [1]
             :bad-value))

  (is (nil? (validate :some-strings ["asdf" "asdf"])))

  (series! :tags :string1
           ::osc/validator a-set)
  (is (code? (validate :tags ["test" :this])
             [1]
             :bad-value))

  (is (code? (validate :tags ["this" "that"])
             :bad-type))

  (is (nil? (validate :tags #{"this" "that"}))))

(deftest test-series-2
  (register-attrs!)
  (rec!    :contact-info-type [:contact-type :contact-value])
  (series! :contact-info-types :contact-info-type)
  (attr!   :contact-infos :contact-info-types)

  (rec!    :person [:given-name :contact-infos])

  (is (code? (validate :person {:given-name "gn"
                                :contact-infos []})
             [:contact-infos]
             :empty-value))

  (is (nil? (validate :person {:given-name "gn"
                               :contact-infos [{:contact-type  :email
                                                :contact-value "some@theplace.com"}]})))

  (is (code? (validate :person {:given-name "gn"
                                :contact-infos [{:contact-type  :emailxx

                                                 :contact-value "some@theplace.com"}]})
             [:contact-infos 0 :contact-type]
             :bad-value))

  (is (code? (validate :person {:given-name "gn"
                                :contact-infos []})
             [:contact-infos]
             :empty-value))

  (is (code? (validate :person {:given-name "gn"
                                :contact-infos [{}]})
             [:contact-infos 0]
             :empty-value)))
