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

  (is (= (write-json :boolean true) true))
  (is (= (write-json :string  "a string") "a string"))
  (is (= (write-json :string1 "a string") "a string"))
  (is (= (write-json :enum-type :test) "TEST"))

  (is (= (read-json :string1 "a string") "a string"))
  (is (= (read-json :boolean true) true))
  (is (= (read-json :enum-type "TEST") :test))

  (is (= (read-json :local-date "2024-01-01")
         (LocalDate/parse "2024-01-01")))
  (is (= (read-json :instant "2024-01-05T23:13:57.254310Z")
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

  (is (= (write-json :given-name {:given-name "my name is ..."}) [:the-given-name "my name is ..."]))
  (is (= (write-json :active?    {:active? true})                [:is-active true]))
  (is (= (write-json :enum       {:enum :test})                  [:enum "TEST"]))

  (is (= (read-json :given-name {:the-given-name "my name is ..."}) [:given-name "my name is ..."]))
  (is (= (read-json :active?    {:is-active true})                  [:active? true]))
  (is (= (read-json :enum       {:enum "TEST"})                     [:enum :test])))

(deftest test-recs
  (register-attrs!)
  (rec! :person1 [:given-name :active? :enum])
  (rec! :person2 [:given-name :active? :enums])

  (let [clj  {:given-name     "Bob" :active?   false :enum :test}
        json {:the-given-name "Bob" :is-active false :enum "TEST"}]
    (is (= (write-json :person1 clj) json))
    (is (= (read-json :person1 json) clj)))

  (let [clj  {:given-name "Bob" :active? false :enums [:one :two]}
        json {:the-given-name "Bob" :is-active false :enums ["ONE" "TWO"]}]
    (is (= (write-json :person2 clj)  json))
    (is (= (read-json  :person2 json) clj))))

(deftest test-nested-recs
  (register-attrs!)

  (scalar! :contact-type-enum global-keyword ::osj/kind ::osj/enum)
  (attr!   :contact-type  :contact-type-enum)
  (attr!   :contact-value :string1)
  (rec!    :contact-info [:contact-type :contact-value])

  (series! :contact-info-series :contact-info)
  (attr!   :contact-infos :contact-info-series)

  (rec! :person [:given-name :active? :contact-infos])
  (is (= (write-json :person {:give-name     "Person 1"
                              :active?       true
                              :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                              {:contact-type :email  :contact-value "blah@blah"}]})
         {:the-given-name nil
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

  (series! :one-two :enum-type)
  (is (= (write-json :one-two [:one :two]) ["ONE" "TWO"]))

  (rec! :person [:given-name :active? :enums])
  (series! :people :person)
  (is (= (write-json :people [{:given-name "Bob"  :active? false :enums [:test]}
                              {:given-name "Jane" :active? true  :enums [:test :this]}])
         [{:the-given-name "Bob"  :is-active false :enums ["TEST"]}
          {:the-given-name "Jane" :is-active true  :enums ["TEST" "THIS"]}])))
