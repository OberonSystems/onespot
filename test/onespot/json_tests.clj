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

  (attr! :enum :enum1))

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
         [:is-active true])))

(deftest rec-tests-1
  (register-attrs!)
  (rec! :person [:given-name :active?])
  (is (= (write-json :person {:given-name "Bob" :active? false})
         {:the-given-name "Bob"
          :is-active false})))

(deftest series-tests-1
  (register-attrs!)
  (series! :strings :string1)
  (is (= (write-json :strings ["one" "two"]) ["one" "two"]))

  (series! :enums :enum1)
  (is (= (write-json :enums [:one :two]) ["ONE" "TWO"]))

  (rec! :person [:given-name :active?])
  (series! :people :person)
  (is (= (write-json :people [{:given-name "Bob"  :active? false}
                              {:given-name "Jane" :active? true}])
         [{:the-given-name "Bob"  :is-active false}
          {:the-given-name "Jane" :is-active true}])))
