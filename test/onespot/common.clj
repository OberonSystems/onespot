(ns onespot.common
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.lacinia  :as osl]
            [onespot.json     :as osj]
            [onespot.entities :as ose]
            :reload))

(def +shirt-size-enums+
  [{:value :sm :description "Small"}
   {:value :md :description "Medium"}
   {:value :lg :description "Large"}
   {:value :xl :description "Extra Large"}])

(def +shirt-sizes+
  (->> +shirt-size-enums+ (map :value) set))

(defn validate-shirt-size
  [x]
  (when-not (+shirt-sizes+ x)
    {:code    :bad-value
     :message (format "Shirt Size must be one of `%s` not `%s`." +shirt-sizes+ x)
     :value   x}))

(defn register-scalars!
  []
  (clear!)
  (ose/register-common!)
  (scalar! :string1 non-blank-string)
  (scalar! :string2 non-blank-string
           ::osc/label       "My Label"
           ::osc/description "My Description")
  ;;
  (let [contact-types #{:mobile :email}]
    (scalar! :contact-type-enum #(one-of % contact-types)
             ::osc/enums contact-types))

  (scalar! :shirt-size-type validate-shirt-size
           ::osc/enums +shirt-size-enums+))

(defn register-attrs!
  []
  (register-scalars!)
  (attr! :person-id   ::osc/positive-integer)
  (attr! :given-name  ::osc/string ::osj/entity-id :the-given-name)
  (attr! :nickname    ::osc/string)
  (attr! :family-name ::osc/string
         ::osc/label "The Family Name")
  ;;
  (attr! :day ::osc/local-date)
  (attr! :now ::osc/instant)
  ;;
  (attr! :contact-type  :contact-type-enum)
  (attr! :contact-value ::osc/string)

  (attr! :active?        ::osc/boolean
         ::osl/entity-id :is-active
         ::osj/entity-id :is-active)

  (attr! :json-active?   ::osc/boolean
         ::osl/entity-id :is-active
         ::osj/entity-id :json-is-active)

  (attr! :shirt-size :shirt-size-type)

  (series! :s/shirt-sizes :shirt-size-type)
  (attr! :shirt-sizes :s/shirt-sizes))

(defn register-all!
  []
  (clear!)
  (register-attrs!)

  (series! :some-strings :string1)
  (series! :tags :string1 ::osc/validator a-set)

  (rec! :person
        [:person-id
         :given-name
         :family-name
         :shirt-size
         :active?]
        ::osc/identity-ids [:person-id])

  (rec! :new-person
        (osc/rec-value-ids :person))

  (series! :people :person)

  (rec! :person-with-output
        [:person-id
         :given-name]
        ::osc/identity-ids [:person-id]
        ::osl/output-ids   [:family-name])

  (rec! :person-with-core-description
        [:person-id :given-name]
        ::osc/identity-ids [:person-id]
        ::osc/description "Core Description")

  (rec! :person-with-lacinia-description
        [:person-id :given-name]
        ::osc/identity-ids [:person-id]
        ::osc/description "Core Description"
        ::osl/description "Lacinia Description"))
