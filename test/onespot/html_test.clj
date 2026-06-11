(ns onespot.html-test
  (:require [clojure.test :refer [deftest is]])
  (:require [onespot.core       :refer [attr! rec! series!] :as os]
            [onespot.html :refer [->clj-keys ->clj-value  ->clj
                                  ->html-keys ->html-value ->html]
             :as ht]
            [onespot.test-utils :refer [register-all! register-attrs! register-scalars!]]
            :reload)
  (:import [java.time LocalDate Instant]))

(deftest test-get-entity-ids
  (register-attrs!)
  (is (= (os/entity-id :given-name) :given-name))
  (is (= (ht/entity-id :given-name) :the-ht-given-name))
  (is (not (os/registered? :the-ht-given-name))))

(deftest test-scalars
  (register-scalars!)

  (is (= (->html-value ::os/boolean true) "true"))
  (is (= (->html-value ::os/boolean false) "false"))
  (is (= (->html-value ::os/boolean (LocalDate/parse "2000-01-01")) "true"))
  (is (= (->html-value ::os/boolean nil) "false"))
  (is (= (->html-value ::os/boolean :anything) "true"))
  (is (= (->html-value ::os/boolean 123) "true"))

  (is (= (->html-value ::os/string "a string") "a string"))
  (is (= (->html-value :string-type1 "a string") "a string"))
  (is (= (->html-value :size-enum :sm) "SM"))

  (is (= (->clj-value ::os/boolean "true") true))
  (is (= (->clj-value :string-type1 "a string") "a string"))
  (is (= (->clj-value :size-enum "SM") :sm))

  (is (= (->clj-value ::os/local-date "2024-01-01")
         (LocalDate/parse "2024-01-01")))
  (is (= (->clj-value ::os/instant "2024-01-05T23:13:57.254310Z")
         (Instant/parse "2024-01-05T23:13:57.254310Z"))))

(deftest test-attrs
  (register-attrs!)
  (let [local-str "2024-01-05"
        local-obj (LocalDate/parse local-str)]
    (is (= (->html-value :day local-obj) local-str))
    (is (= (->clj-value :day local-str) local-obj)))

  (let [inst-str "2024-01-05T23:13:57.254310Z"
        inst-obj (Instant/parse inst-str)]
    (is (= (->html-value :now inst-obj) inst-str))
    (is (= (->clj-value  :now inst-str) inst-obj)))

  (is (= (->html-value :given-name "my name is ...") "my name is ..."))
  (is (= (->html-value :active?    true)             "true"))
  (is (= (->html-value :size       :sm)              "SM"))

  (is (= (->clj-value :given-name "my name is ...") "my name is ..."))
  (is (= (->clj-value :active?    "true")           true))
  (is (= (->clj-value :size       "SM")             :sm)))

(deftest test-recs
  (register-all!)
  (rec! :person1 [:given-name :active? :size])
  (rec! :person2 [:given-name :active? :sizes])

  ;; Only checking key coercion, make it obvious the values are irrelevant
  (let [clj  {:given-name        :_ :active?   :_ :size :_}
        html {:the-ht-given-name :_ :is-active :_ :size :_}]
    (is (= (->html-keys clj)  html))
    (is (= (->clj-keys  html) clj)))

  (let [clj  {:given-name        "Bob" :active?   false :sizes [:sm :lg]}
        html {:the-ht-given-name "Bob" :is-active "false" :sizes  ["SM" "LG"]}]
    (is (= (->> (->html-value :person2 clj) ->html-keys) html))
    (is (= (->> html ->clj-keys (->clj-value :person2))  clj)))

  ;; Reading/Writing when entity has additional readonly attributes
  (is (= (->> (->html-value :person-with-readonly {:person-id 1234 :given-name "Bob" :family-name "Jane"})
              ->html-keys)
         {:person-id 1234 :the-ht-given-name "Bob" :family-name "Jane"}))

  (is (= (->> (->html-value :person-with-readonly {:person-id 1234 :given-name "Bob"})
              ->html-keys)
         {:person-id 1234 :the-ht-given-name "Bob" :family-name nil}))

  (is (= (->> (->html-value :person-with-readonly {:person-id 1234})
              ->html-keys)
         {:person-id 1234 :the-ht-given-name nil :family-name nil}))
  ;;
  (is (= (->> {:person-id 1234 :the-ht-given-name "Bob" :family-Name "Jane"}
              ->clj-keys
              (->clj-value :person-with-readonly))
         {:person-id 1234 :given-name "Bob"}))

  (is (= (->> {:person-id 1234}
              ->clj-keys
              (->clj-value :person-with-readonly))
         {:person-id 1234 :given-name nil})))

(deftest test-nested-recs
  (register-attrs!)
  (rec!    :contact-info    [:contact-type :contact-value])
  (series! :s/contact-infos :contact-info)
  (attr!   :contact-infos :s/contact-infos)

  (rec! :person [:given-name :active? :contact-infos])
  (is (= (->> {:given-name    "Person 1"
               :active?       true
               :contact-infos [{:contact-type :mobile :contact-value "1234"}
                               {:contact-type :email  :contact-value "blah@blah"}]}
              (->html-value :person)
              ->html-keys)
         {:the-ht-given-name "Person 1"
          :is-active     "true"
          :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                          {:contact-type "EMAIL"  :contact-value "blah@blah"}]}))

  (series! :people-series :person)
  (is (= (->> [{:given-name    "Person 1"
                :active?       true
                :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                {:contact-type :email  :contact-value "blah@blah"}]}
               {:given-name    "Person 2"
                :active?       true
                :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                {:contact-type :email  :contact-value "blah@blah"}]}]
              (->html-value :people-series)
              ->html-keys)
         [{:the-ht-given-name "Person 1"
           :is-active     "true"
           :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                           {:contact-type "EMAIL"  :contact-value "blah@blah"}]}
          {:the-ht-given-name "Person 2"
           :is-active     "true"
           :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                           {:contact-type "EMAIL"  :contact-value "blah@blah"}]}]))

  (attr! :people :people-series)
  (rec! :address-book [:people])
  (is (= (->> {:people [{:given-name    "Person 1"
                         :active?       true
                         :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                         {:contact-type :email  :contact-value "blah@blah"}]}
                        {:given-name    "Person 2"
                         :active?       true
                         :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                         {:contact-type :email  :contact-value "blah@blah"}]}]}
              (->html-value :address-book)
              ->html-keys)
         {:people [{:the-ht-given-name "Person 1"
                    :is-active     "true"
                    :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                                    {:contact-type "EMAIL"  :contact-value "blah@blah"}]}
                   {:the-ht-given-name "Person 2"
                    :is-active     "true"
                    :contact-infos [{:contact-type "MOBILE" :contact-value "1234"}
                                    {:contact-type "EMAIL"  :contact-value "blah@blah"}]}]})))

(deftest test-series
  (register-attrs!)
  (series! :strings :string-type1)
  (is (= (->html-value :strings ["one" "two"]) ["one" "two"]))

  (series! :booleans ::os/boolean)
  (is (= (->html-value :booleans [true true false]) ["true" "true" "false"]))

  (rec! :person [:given-name :active? :sizes])
  (series! :people :person)
  (is (= (->> [{:given-name "Bob"  :active? false :sizes [:sm :lg]}
               {:given-name "Jane" :active? true  :sizes [:sm :xl]}]
              (->html-value :people)
              ->html-keys)
         [{:the-ht-given-name "Bob"  :is-active "false" :sizes ["SM" "LG"]}
          {:the-ht-given-name "Jane" :is-active "true"  :sizes ["SM" "XL"]}])))
