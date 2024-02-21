(ns onespot.json-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.json :as osj :refer [->core-keys ->core
                                          ->json-keys ->json]]
            [onespot.common :refer :all]
            :reload)
  (:import [java.time LocalDate Instant]))

(deftest test-scalars
  (register-scalars!)

  (is (= (->json ::osc/boolean true) true))
  (is (= (->json ::osc/string "a string") "a string"))
  (is (= (->json :string1 "a string") "a string"))
  (is (= (->json :shirt-size-type :sm) "SM"))

  (is (= (->core ::osc/boolean true) true))
  (is (= (->core :string1 "a string") "a string"))
  (is (= (->core :shirt-size-type "SM") :sm))

  (is (= (->core ::osc/local-date "2024-01-01")
         (LocalDate/parse "2024-01-01")))
  (is (= (->core ::osc/instant "2024-01-05T23:13:57.254310Z")
         (Instant/parse "2024-01-05T23:13:57.254310Z"))))

(deftest test-attrs
  (register-attrs!)
  (let [local-str "2024-01-05"
        local-obj (LocalDate/parse local-str)]
    (is (= (->json :day local-obj) local-str))
    (is (= (->core :day local-str) local-obj)))

  (let [inst-str "2024-01-05T23:13:57.254310Z"
        inst-obj (Instant/parse inst-str)]
    (is (= (->json :now inst-obj) inst-str))
    (is (= (->core :now inst-str) inst-obj)))

  (is (= (->json :given-name   "my name is ...") "my name is ..."))
  (is (= (->json :active?      true)             true))
  (is (= (->json :shirt-size   :sm)              "SM"))

  (is (= (->core :given-name   "my name is ...") "my name is ..."))
  (is (= (->core :active?      true)             true))
  (is (= (->core :shirt-size   "SM")             :sm)))

(deftest test-recs
  (register-all!)
  (rec! :person1 [:given-name :active? :shirt-size])
  (rec! :person2 [:given-name :active? :shirt-sizes])

  (let [core {:given-name   "Bob" :active?  false :shirt-size :sm}
        json {:theGivenName "Bob" :isActive false :shirtSize  :sm}]
    (is (= (->json-keys core) json))
    (is (= (->core-keys json) core)))

  (let [core {:given-name   "Bob" :active?  false :shirt-sizes [:sm :lg]}
        json {:theGivenName "Bob" :isActive false :shirtSizes  ["SM" "LG"]}]
    (is (= (->> (->json :person2 core) ->json-keys)  json))
    (is (= (->> json ->core-keys (->core :person2)) core)))

  ;; Reading/Writing when entity has additional output attributes
  (is (= (->> (->json :person-with-output {:person-id 1234 :given-name "Bob" :family-name "Jane"})
              ->json-keys)
         {:personId 1234 :theGivenName "Bob" :familyName "Jane"}))

  (is (= (->> (->json :person-with-output {:person-id 1234 :given-name "Bob"})
              ->json-keys)
         {:personId 1234 :theGivenName "Bob" :familyName nil}))

  (is (= (->> (->json :person-with-output {:person-id 1234})
              ->json-keys)
         {:personId 1234 :theGivenName nil :familyName nil}))
  ;;
  (is (= (->> {:personId 1234 :theGivenName "Bob" :familyName "Jane"}
              ->core-keys
              (->core :person-with-output) )
         {:person-id 1234 :given-name "Bob"}))

  (is (= (->> {:person-id 1234}
              ->core-keys
              (->core :person-with-output))
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
              (->json :person)
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
              (->json :people-series)
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
              (->json :address-book)
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
  (is (= (->json :strings ["one" "two"]) ["one" "two"]))

  (series! :booleans ::osc/boolean)
  (is (= (->json :booleans [true true false]) [true true false]))

  (rec! :person [:given-name :active? :shirt-sizes])
  (series! :people :person)
  (is (= (->> [{:given-name "Bob"  :active? false :shirt-sizes [:sm :lg]}
               {:given-name "Jane" :active? true  :shirt-sizes [:sm :xl]}]
              (->json :people)
              ->json-keys)
         [{:theGivenName "Bob"  :isActive false :shirtSizes ["SM" "LG"]}
          {:theGivenName "Jane" :isActive true  :shirtSizes ["SM" "XL"]}])))
