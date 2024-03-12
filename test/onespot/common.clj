(ns onespot.common
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.lacinia  :as lc]
            [onespot.json     :as js]
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
           :label       "My Label"
           :description "My Description")
  ;;
  (let [contact-types #{:mobile :email}]
    (scalar! :contact-type-enum #(one-of % contact-types)
             :enums contact-types))

  (scalar! :shirt-size-type validate-shirt-size
           :enums +shirt-size-enums+))

(defn register-attrs!
  []
  (register-scalars!)
  (attr! :person-id   ::osc/positive-integer)
  (attr! :given-name  ::osc/string ::js/entity-id :theGivenName)
  (attr! :nickname    ::osc/string)
  (attr! :family-name ::osc/string
         :label "The Family Name")
  ;;
  (attr! :day ::osc/local-date)
  (attr! :dob ::osc/local-date)
  (attr! :now ::osc/instant)
  ;;
  (attr! :contact-type  :contact-type-enum)
  (attr! :contact-value ::osc/string)

  (attr! :active?        ::osc/boolean
         ::lc/entity-id :isActive
         ::js/entity-id :isActive)

  (attr! :shirt-size :shirt-size-type)

  (series! :s/shirt-sizes :shirt-size-type)
  (attr! :shirt-sizes :s/shirt-sizes))

(defn register-all!
  []
  (clear!)
  (register-attrs!)

  (series! :some-strings :string1)
  (series! :tags :string1 :validator a-set)

  (rec! :person
        [:person-id
         :given-name
         :family-name
         :shirt-size
         :dob
         :active?]
        :identity-ids [:person-id]
        :optional-ids [:dob])

  (rec! :new-person
        (osc/rec-value-ids :person))

  (series! :people :person)

  (rec! :person-with-readonly
        [:person-id :given-name]
        :identity-ids [:person-id]
        :readonly-ids [:family-name])

  (rec! :person-with-readonly
        [:person-id
         :given-name]
        :identity-ids [:person-id]
        :readonly-ids [:family-name])

  (rec! :person-with-optional-fields
        [:person-id :given-name :family-name]
        :identity-ids [:person-id]
        :optional-ids [:given-name])

  (rec! :person-with-core-description
        [:person-id :given-name]
        :identity-ids [:person-id]
        :description "Core Description")

  (rec! :person-with-lacinia-description
        [:person-id :given-name]
        :identity-ids [:person-id]
        :description "Core Description"
        ::lc/info {:description "Lacinia Description"}))
