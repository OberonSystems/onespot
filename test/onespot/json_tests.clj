(ns onespot.json-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.json :as osj :refer [write-json read-json]]
            :reload))

(defn register-scalars!
  []
  (clear!)
  (scalar! :string1 non-blank-string?)
  (scalar! :enum1 global-keyword ::osj/kind ::osj/enum)
  (scalar! :boolean true-or-false))

(defn register-attrs!
  []
  (register-scalars!)
  (attr! :given-name :string1
         ::osj/json-id :the-given-name)

  (attr! :active? :boolean
         ::osj/json-id :is-active)

  (attr! :enum :enum1)

  (series! :enum1s :enum1)
  (attr! :enums :enum1s))

;;;

(deftest scalar-tests-1
  (register-scalars!)

  (is (= (write-json :string1 "a string")))
  (is (= (write-json :enum1 :test) "TEST")))

(deftest attr-tests-1
  (register-attrs!)

  (is (= (write-json :given-name {:given-name "my name is ..."})
         [:the-given-name "my name is ..."]))
  (is (= (write-json :active? {:active? true})
         [:is-active true]))

  (is (= (write-json :enum {:enum :test}) [:enum "TEST"])))

(deftest rec-tests-1
  (register-attrs!)

  (rec! :person [:given-name :active? :enum])
  (is (= (write-json :person {:given-name "Bob"
                              :active? false
                              :enum :test})
         {:the-given-name "Bob"
          :is-active      false
          :enum           "TEST"}))

  (rec! :person [:given-name :active? :enums])
  (is (= (write-json :person {:given-name "Bob" :active? false :enums [:one :two]})
         {:the-given-name "Bob" :is-active false :enums ["ONE" "TWO"]})))

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

(deftest series-tests-1
  (register-attrs!)

  (series! :strings :string1)
  (is (= (write-json :strings ["one" "two"]) ["one" "two"]))

  (series! :one-two :enum1)
  (is (= (write-json :one-two [:one :two]) ["ONE" "TWO"]))

  (rec! :person [:given-name :active? :enums])
  (series! :people :person)
  (is (= (write-json :people [{:given-name "Bob"  :active? false :enums [:test]}
                              {:given-name "Jane" :active? true  :enums [:test :this]}])
         [{:the-given-name "Bob"  :is-active false :enums ["TEST"]}
          {:the-given-name "Jane" :is-active true  :enums ["TEST" "THIS"]}])))
