(ns onespot.json-test
  (:require [clojure.test :refer [deftest is]])
  (:require [onespot.core       :refer [attr! rec! series!] :as os]
            [onespot.json       :refer [->clj-keys ->clj-value  ->clj
                                        ->json-keys ->json-value ->json]]
            [onespot.test-utils :refer [register-all! register-attrs! register-scalars!]]
            :reload)
  (:import [java.time LocalDate Instant]))

(deftest test-scalars
  (register-scalars!)

  (is (= (->json-value ::os/boolean true) true))
  (is (= (->json-value ::os/string "a string") "a string"))
  (is (= (->json-value :string1 "a string") "a string"))
  (is (= (->json-value :shirt-size-type :sm) "SM"))

  (is (= (->clj-value ::os/boolean true) true))
  (is (= (->clj-value :string1 "a string") "a string"))
  (is (= (->clj-value :shirt-size-type "SM") :sm))

  (is (= (->clj-value ::os/local-date "2024-01-01")
         (LocalDate/parse "2024-01-01")))
  (is (= (->clj-value ::os/instant "2024-01-05T23:13:57.254310Z")
         (Instant/parse "2024-01-05T23:13:57.254310Z"))))

(deftest test-attrs
  (register-attrs!)
  (let [local-str "2024-01-05"
        local-obj (LocalDate/parse local-str)]
    (is (= (->json-value :day local-obj) local-str))
    (is (= (->clj-value :day local-str) local-obj)))

  (let [inst-str "2024-01-05T23:13:57.254310Z"
        inst-obj (Instant/parse inst-str)]
    (is (= (->json-value :now inst-obj) inst-str))
    (is (= (->clj-value  :now inst-str) inst-obj)))

  (is (= (->json-value :given-name   "my name is ...") "my name is ..."))
  (is (= (->json-value :active?      true)             true))
  (is (= (->json-value :shirt-size   :sm)              "SM"))

  (is (= (->clj-value :given-name   "my name is ...") "my name is ..."))
  (is (= (->clj-value :active?      true)             true))
  (is (= (->clj-value :shirt-size   "SM")             :sm)))

(deftest test-recs
  (register-all!)
  (rec! :person1 [:given-name :active? :shirt-size])
  (rec! :person2 [:given-name :active? :shirt-sizes])

  (let [core {:given-name   "Bob" :active?  false :shirt-size :sm}
        json {:theGivenName "Bob" :isActive false :shirtSize  :sm}]
    (is (= (->json-keys core) json))
    (is (= (->clj-keys json) core)))

  (let [core {:given-name   "Bob" :active?  false :shirt-sizes [:sm :lg]}
        json {:theGivenName "Bob" :isActive false :shirtSizes  ["SM" "LG"]}]
    (is (= (->> (->json-value :person2 core) ->json-keys)  json))
    (is (= (->> json ->clj-keys (->clj-value :person2)) core)))

  ;; Reading/Writing when entity has additional readonly attributes
  (is (= (->> (->json-value :person-with-readonly {:person-id 1234 :given-name "Bob" :family-name "Jane"})
              ->json-keys)
         {:personId 1234 :theGivenName "Bob" :familyName "Jane"}))

  (is (= (->> (->json-value :person-with-readonly {:person-id 1234 :given-name "Bob"})
              ->json-keys)
         {:personId 1234 :theGivenName "Bob" :familyName nil}))

  (is (= (->> (->json-value :person-with-readonly {:person-id 1234})
              ->json-keys)
         {:personId 1234 :theGivenName nil :familyName nil}))
  ;;
  (is (= (->> {:personId 1234 :theGivenName "Bob" :familyName "Jane"}
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
              (->json-value :person)
              ->json-keys)
         {:theGivenName "Person 1"
          :isActive     true
          :contactInfos [{:contactType "MOBILE" :contactValue "1234"}
                         {:contactType "EMAIL"  :contactValue "blah@blah"}]}))

  (series! :people-series :person)
  (is (= (->> [{:given-name    "Person 1"
                :active?       true
                :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                {:contact-type :email  :contact-value "blah@blah"}]}
               {:given-name    "Person 2"
                :active?       true
                :contact-infos [{:contact-type :mobile :contact-value "1234"}
                                {:contact-type :email  :contact-value "blah@blah"}]}]
              (->json-value :people-series)
              ->json-keys)
         [{:theGivenName "Person 1"
           :isActive     true
           :contactInfos [{:contactType "MOBILE" :contactValue "1234"}
                          {:contactType "EMAIL"  :contactValue "blah@blah"}]}
          {:theGivenName "Person 2"
           :isActive       true
           :contactInfos  [{:contactType "MOBILE" :contactValue "1234"}
                           {:contactType "EMAIL"  :contactValue "blah@blah"}]}]))

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
              (->json-value :address-book)
              ->json-keys)
         {:people [{:theGivenName "Person 1"
                    :isActive      true
                    :contactInfos [{:contactType "MOBILE" :contactValue "1234"}
                                   {:contactType "EMAIL"  :contactValue "blah@blah"}]}
                   {:theGivenName "Person 2"
                    :isActive      true
                    :contactInfos [{:contactType "MOBILE" :contactValue "1234"}
                                   {:contactType "EMAIL"  :contactValue "blah@blah"}]}]})))

(deftest test-series
  (register-attrs!)
  (series! :strings :string1)
  (is (= (->json-value :strings ["one" "two"]) ["one" "two"]))

  (series! :booleans ::os/boolean)
  (is (= (->json-value :booleans [true true false]) [true true false]))

  (rec! :person [:given-name :active? :shirt-sizes])
  (series! :people :person)
  (is (= (->> [{:given-name "Bob"  :active? false :shirt-sizes [:sm :lg]}
               {:given-name "Jane" :active? true  :shirt-sizes [:sm :xl]}]
              (->json-value :people)
              ->json-keys)
         [{:theGivenName "Bob"  :isActive false :shirtSizes ["SM" "LG"]}
          {:theGivenName "Jane" :isActive true  :shirtSizes ["SM" "XL"]}])))
