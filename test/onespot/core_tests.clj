(ns onespot.core-tests
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as os]
            [onespot.validators :refer :all]
            [onespot.validate :refer [validate]]
            ;;
            [onespot.common :refer :all]
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

;;; --------------------------------------------------------------------------------

(deftest test-scalars-1
  (register-scalars!)
  (is (= (label     :string1) "String1"))
  (is (= (validator :string1) non-blank-string))

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

        :identity-ids [:person-id]
        :optional-ids [:nickname]
        ;;
        ::something-else :hi-there)

  (is (= (os/rec-attr-ids :person)
         [:person-id :given-name :nickname :family-name]))

  (is (= (os/rec-optional-set :person)
         #{:nickname}))

  (is (= (os/rec-content :person {:person-id 123 :given-name "given" :family-name "family"
                                   :other-stuff :that :gets :ignored})
         {:person-id 123 :given-name "given", :family-name "family"}))

  (is (= (os/rec-identity :person {:person-id 123 :given-name "given" :family-name "family"})
         {:person-id 123}))

  (is (= (os/rec-values :person {:person-id 123 :given-name "given" :family-name "family"})
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
        :identity-ids [:person-id])

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
        :identity-ids [:person-id]
        :optional-ids [:contact-info]
        :validator    (fn [{:keys [family-name]}]
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
  (register-all!)

  (is (code? (validate :some-strings nil)
             :missing-value))

  (is (code? (validate :some-strings [])
             :empty-value))

  (is (code? (validate :some-strings ["test" :this])
             [1]
             :bad-value))

  (is (nil? (validate :some-strings ["asdf" "asdf"])))

  ;; Should fail as `:this` should be a string.
  (is (code? (validate :tags ["test" :this])
             [1]
             :bad-value))

  ;; Should fail as it isn't a set.
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

(deftest test-walking-recs
  (register-all!)
  (is (= (walk-entities :given-name)
         #{:given-name ::os/string}))

  (is (= (walk-entities :person)
         #{:onespot.core/positive-integer
           :active?
           :person
           :person-id
           :onespot.core/string
           :given-name
           :onespot.core/local-date
           :dob
           :shirt-size-type
           :shirt-size
           :family-name
           :onespot.core/boolean}))

  ;; Walking :person or :people should be the same as :person is
  ;; contained in :people so it's a referenced type and should be
  ;; included either way.
  (is (= (walk-entities :people)
         (walk-entities [:person :people])))

  (is (= (rec-attr-ids :person-with-readonly)                 [:person-id :given-name]))
  (is (= (rec-attr-ids :person-with-readonly :readonly? true) [:person-id :given-name :family-name]))

  (is (= (walk-entities :person-with-readonly)
         #{:onespot.core/positive-integer
           :person-id
           :onespot.core/string
           :person-with-readonly
           :given-name})))

(deftest test-enums
  (is (= (os/canonicalise-enums [:test :this])
         [{:value :test} {:value :this}]))

  (is (= (os/canonicalise-enums [[:value1 :desc1]
                                  [:value2]
                                  [:value3 :desc3]
                                  :value4])
         [{:value :value1 :description :desc1}
          {:value :value2}
          {:value :value3 :description :desc3}
          {:value :value4}])))
