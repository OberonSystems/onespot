(ns onespot.core-test
  (:require [clojure.test :refer [deftest is]])
  (:require [onespot.core :as os]
            [onespot.validators :as vl]
            [onespot.validate :refer [validate]]
            [onespot.test-utils :refer [register-all! register-attrs! register-scalars!]]
            :reload))

(defn err?
  [validation & {:keys [path code value]}]
  (and (or (nil? path)  (= path  (-> validation first :path)))
       (or (nil? code)  (= code  (-> validation first :feedback :code)))
       (or (nil? value) (= value (-> validation first :feedback :value)))))
;;; --------------------------------------------------------------------------------

(deftest test-scalars-1
  (register-scalars!)
  (is (= (os/label       :string-type1) "String Type1"))
  (is (= (os/description :string-type1) nil))
  (is (= (os/validator   :string-type1) vl/non-blank-string))

  (is (= (os/label       :string-type2) "My Label For String Type 2"))
  (is (= (os/description :string-type2) "A String Type 2"))

  (is (err? (validate :string-type1 nil) :code :missing-value))
  (is (err? (validate :string-type1 "")  :code :bad-value))
  (is (nil? (validate :string-type1 "a string"))))

(deftest test-attrs-1
  (register-attrs!)

  (is (= (os/label :given-name) "Given Name"))
  (is (= (os/label :family-name) "The Family Name"))
  ;;
  (is (err? (validate :given-name nil)               :code :missing-value))
  (is (err? (validate :given-name {})                :code :missing-value))
  (is (err? (validate :given-name {:given-name nil}) :code :missing-value))
  (is (nil? (validate :given-name {:given-name "a given name"}))))

(deftest test-recs-1
  (register-attrs!)
  (os/rec! :person
           [:person-id :given-name :nickname :family-name]
           :identity-ids [:person-id]
           :optional-ids [:nickname]
           ::something-else :hi-there)

  (is (= (os/rec-attr-ids :person)
         [:person-id :given-name :nickname :family-name]))

  (is (= (os/rec-optional-set :person)
         #{:nickname}))

  (is (= (os/rec-content :person {:person-id 123 :given-name "given" :family-name "family"
                                  :other-stuff :that :gets :ignored})
         {:person-id 123
          :given-name "given"
          :family-name "family"}))

  (is (= (os/rec-identity :person {:person-id 123 :given-name "given" :family-name "family"})
         {:person-id 123}))

  (is (= (os/rec-values :person {:person-id 123 :given-name "given" :family-name "family"})
         {:given-name "given"
          :family-name "family"}))

  (is (err? (validate :person {:person-id   "my-id"
                               :given-name  "g"
                               :nickname    "n"
                               :family-name "f"})
            :path [:person-id]
            :code :bad-value
            :value "my-id"))

  (is (err? (validate :person {:person-id   10
                               :given-name  nil
                               :nickname    "n"
                               :family-name "f"})
            :path [:given-name]
            :code :missing-value))
  (let [errs (validate :person {:given-name  nil
                                :nickname    "n"
                                :family-name "f"})]
    (is (err? errs
              ; FIXME: This should include the path
              ;; :path [:person-id]
              :code :missing-attr))
    (is (err? (drop 1 errs)
              :path [:given-name]
              :code :missing-value)))

  (is (nil? (validate :person {:person-id   10
                               :given-name  "g"
                               :nickname    "n"
                               :family-name "f"}))))

(deftest test-recs-nested-1
  (register-attrs!)
  (os/rec!  :contact-info-type [:contact-type :contact-value])
  (os/attr! :contact-info :contact-info-type)

  (os/rec! :person
           [:person-id :given-name :family-name :contact-info]
           :identity-ids [:person-id])

  (is (err? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info {:contact-type :mobilecc
                                              :contact-value "0123 123 123"}})
            :code :bad-value))

  (is (err? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info nil})
            :path [:contact-info]
            :code :missing-value))

  (is (nil? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info {:contact-type :mobile
                                              :contact-value "0123 123 123"}}))))

(deftest test-recs-nested-2
  (register-attrs!)
  (os/rec!  :contact-info-type [:contact-type :contact-value])
  (os/attr! :contact-info :contact-info-type)

  (os/rec! :person
           [:person-id :given-name :family-name :contact-info]
           :identity-ids [:person-id]
           :optional-ids [:contact-info]
           :validator    (fn [{:keys [family-name]}]
                           (when-not (= family-name "fn")
                             {:code :bad-value
                              :message "Family Name must be 'fn'"
                              :value family-name})))

  (is (err? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info {:contact-type :mobilecc
                                              :contact-value "0123 123 123"}})
            :code :bad-value))

  (is (nil? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info {:contact-type :mobile
                                              :contact-value "0123 123 123"}})))

  (is (nil? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "fn"
                               :contact-info nil})))

  (is (err? (validate :person {:person-id 1234
                               :given-name "gn"
                               :family-name "FN-BAD"
                               :contact-info nil})
            :code :bad-value)))

(deftest test-series-1
  (register-all!)

  (is (err? (validate :some-strings nil)
            :code :missing-value))

  (is (err? (validate :some-strings [])
            :code :empty-value))

  (is (err? (validate :some-strings ["test" :this])
            :path [1]
            :code :bad-value))

  (is (nil? (validate :some-strings ["asdf" "asdf"])))

  ;; Should fail as `:this` should be a string.
  (is (err? (validate :tags ["test" :this])
            :path [1]
            :code :bad-value))

  ;; Should fail as it isn't a set.
  (is (err? (validate :tags ["this" "that"])
            :code :bad-type))

  (is (nil? (validate :tags #{"this" "that"}))))

(deftest test-series-2
  (register-attrs!)
  (os/rec!    :contact-info-type [:contact-type :contact-value])
  (os/series! :contact-info-types :contact-info-type)
  (os/attr!   :contact-infos :contact-info-types)

  (os/rec!    :person [:given-name :contact-infos])

  (is (err? (validate :person {:given-name "gn"
                               :contact-infos []})
            :path [:contact-infos]
            :code :empty-value))

  (is (nil? (validate :person {:given-name "gn"
                               :contact-infos [{:contact-type  :email
                                                :contact-value "some@theplace.com"}]})))

  (is (err? (validate :person {:given-name "gn"
                               :contact-infos [{:contact-type  :emailxx
                                                :contact-value "some@theplace.com"}]})
            :path [:contact-infos 0 :contact-type]
            :code :bad-value))

  (is (err? (validate :person {:given-name "gn"
                               :contact-infos []})
            :path [:contact-infos]
            :code :empty-value))

  (is (err? (validate :person {:given-name "gn"
                               :contact-infos [{}]})
            :path [:contact-infos 0]
            :code :empty-value)))

(deftest test-walking-recs
  (register-all!)
  (is (= (os/walk-entities :given-name)
         #{:given-name ::os/string}))

  (is (= (os/walk-entities :person)
         #{:onespot.core/positive-integer
           :active?
           :person
           :person-id
           :onespot.core/string
           :given-name
           :size
           :onespot.core/local-date
           :dob
           :size-enum
           :family-name
           :onespot.core/boolean}))

  ;; Walking :person or :people should be the same as :person is
  ;; contained in :people so it's a referenced type and should be
  ;; included either way.
  (is (= (os/walk-entities :people)
         (os/walk-entities [:person :people])))

  (is (= (os/rec-attr-ids :person-with-readonly)                 [:person-id :given-name]))
  (is (= (os/rec-attr-ids :person-with-readonly :readonly? true) [:person-id :given-name :family-name]))

  (is (= (os/walk-entities :person-with-readonly)
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
