(ns onespot.json-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.json :as osj :refer [write-json read-json]]
            [onespot.common :refer :all]
            :reload)
  (:import [java.time LocalDate Instant]))

(deftest test-scalars
  (register-scalars!)

  (is (= (write-json ::osc/boolean true) true))
  (is (= (write-json ::osc/string "a string") "a string"))
  (is (= (write-json :string1 "a string") "a string"))
  (is (= (write-json :shirt-size-type :sm) "SM"))

  (is (= (read-json ::osc/boolean true) true))
  (is (= (read-json :string1 "a string") "a string"))
  (is (= (read-json :shirt-size-type "SM") :sm))

  (is (= (read-json ::osc/local-date "2024-01-01")
         (LocalDate/parse "2024-01-01")))
  (is (= (read-json ::osc/instant "2024-01-05T23:13:57.254310Z")
         (Instant/parse "2024-01-05T23:13:57.254310Z"))))

(deftest test-attrs
  (register-attrs!)
  (let [local-str "2024-01-05"
        local-obj (LocalDate/parse local-str)]
    (is (= (write-json :day {:day local-obj}) [:day local-str]))
    (is (= (read-json  :day {:day local-str}) [:day local-obj])))

  (let [inst-str "2024-01-05T23:13:57.254310Z"
        inst-obj (Instant/parse inst-str)]
    (is (= (write-json :now {:now inst-obj}) [:now inst-str]))
    (is (= (read-json  :now {:now inst-str}) [:now inst-obj])))

  (is (= (write-json :given-name   {:given-name "my name is ..."}) [:the-given-name "my name is ..."]))
  (is (= (write-json :active?      {:active? true})                [:is-active true]))
  (is (= (write-json :json-active? {:json-active? true})           [:json-is-active true]))
  (is (= (write-json :shirt-size   {:shirt-size :sm})              [:shirt-size "SM"]))

  (is (= (read-json :given-name   {:the-given-name "my name is ..."}) [:given-name "my name is ..."]))
  (is (= (read-json :active?      {:is-active true})                  [:active? true]))
  (is (= (read-json :json-active? {:json-is-active true})             [:json-active? true]))
  (is (= (read-json :shirt-size   {:shirt-size "SM"})                 [:shirt-size :sm])))

(deftest test-recs
  (register-all!)
  (rec! :person1 [:given-name :active? :shirt-size])
  (rec! :person2 [:given-name :active? :shirt-sizes])

  (let [clj  {:given-name     "Bob" :active?   false :shirt-size :sm}
        json {:the-given-name "Bob" :is-active false :shirt-size "SM"}]
    (is (= (write-json :person1 clj) json))
    (is (= (read-json :person1 json) clj)))

  (let [clj  {:given-name "Bob" :active? false :shirt-sizes [:sm :lg]}
        json {:the-given-name "Bob" :is-active false :shirt-sizes ["SM" "LG"]}]
    (is (= (write-json :person2 clj)  json))
    (is (= (read-json  :person2 json) clj)))

  ;; Reading/Writing when entity has additional output attributes
  (is (= (write-json :person-with-output {:person-id 1234 :given-name "Bob" :family-name "Jane"})
         {:person-id 1234 :the-given-name "Bob" :family-name "Jane"}))

  (is (= (write-json :person-with-output {:person-id 1234 :given-name "Bob"})
         {:person-id 1234 :the-given-name "Bob" :family-name nil}))

  (is (= (write-json :person-with-output {:person-id 1234})
         {:person-id 1234 :the-given-name nil :family-name nil}))
  ;;
  (is (= (read-json :person-with-output {:person-id 1234 :the-given-name "Bob" :family-name "Jane"})
         {:person-id 1234, :given-name "Bob"}))

  (is (= (read-json :person-with-output {:person-id 1234})
         {:person-id 1234, :given-name nil})))

(deftest test-nested-recs
  (register-attrs!)
  (rec!    :contact-info    [:contact-type :contact-value])
  (series! :s/contact-infos :contact-info)
  (attr!   :contact-infos :s/contact-infos)

  (rec! :person [:given-name :active? :contact-infos])
  (is (= (write-json :person {:given-name    "Person 1"
                              :active?       true
                              :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                              {:contact-type :email  :contact-value "blah@blah"}]})
         {:the-given-name "Person 1"
          :is-active      true
          :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                          {:contact-type "EMAIL"  :contact-value "blah@blah"}]}))

  (series! :people-series :person)
  (is (= (write-json :people-series [{:give-name     "Person 1"
                                      :active?       true
                                      :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                                      {:contact-type :email  :contact-value "blah@blah"}]}
                                     {:give-name     "Person 2"
                                      :active?       true
                                      :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                                      {:contact-type :email  :contact-value "blah@blah"}]}])
         [{:the-given-name nil
           :is-active      true
           :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                           {:contact-type "EMAIL"  :contact-value "blah@blah"}]}
          {:the-given-name nil
           :is-active      true
           :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                           {:contact-type "EMAIL"  :contact-value "blah@blah"}]}]))

  (attr! :people :people-series)
  (rec! :address-book [:people])
  (is (= (write-json :address-book {:people [{:give-name     "Person 1"
                                              :active?       true
                                              :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                                              {:contact-type :email  :contact-value "blah@blah"}]}
                                             {:give-name     "Person 2"
                                              :active?       true
                                              :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                                              {:contact-type :email  :contact-value "blah@blah"}]}]})
         {:people [{:the-given-name nil
                    :is-active      true
                    :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                                    {:contact-type "EMAIL"  :contact-value "blah@blah"}]}
                   {:the-given-name nil
                    :is-active      true
                    :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                                    {:contact-type "EMAIL"  :contact-value "blah@blah"}]}]})))

(deftest test-series
  (register-attrs!)

  (series! :strings :string1)
  (is (= (write-json :strings ["one" "two"]) ["one" "two"]))

  (series! :booleans ::osc/boolean)
  (is (= (write-json :booleans [true true false]) [true true false]))

  (rec! :person [:given-name :active? :shirt-sizes])
  (series! :people :person)
  (is (= (write-json :people [{:given-name "Bob"  :active? false :shirt-sizes [:sm :lg]}
                              {:given-name "Jane" :active? true  :shirt-sizes [:sm :xl]}])
         [{:the-given-name "Bob"  :is-active false :shirt-sizes ["SM" "LG"]}
          {:the-given-name "Jane" :is-active true  :shirt-sizes ["SM" "XL"]}])))
