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

  (scalar! :positive-integer positive-integer ::osl/gql-type :int)
  (scalar! :string           non-blank-string)
  (scalar! :boolean          true-or-false)
  (scalar! :enum-type        global-keyword ::osj/kind ::osj/enum)
  (scalar! :shirt-size-type  validate-shirt-size
           ::osc/enums +shirt-size-enums+
           ::osj/kind  ::osj/enum))

(defn register-attrs!
  []
  (register-scalars!)
  (attr! :person-id   :positive-integer)
  (attr! :given-name  :string ::osj/json-id :the-given-name)
  (attr! :nickname    :string)
  (attr! :family-name :string
         ::osc/label "The Family Name")
  ;;
  (attr! :day :local-date)
  (attr! :now :instant)
  ;;
  (attr! :contact-type  :contact-type-enum)
  (attr! :contact-value :string1)

  (attr! :active?      :boolean
         ::osl/gql-id  :is-active
         ::osj/json-id :is-active)

  (attr! :json-active? :boolean
         ::osl/gql-id  :is-active
         ::osj/json-id :json-is-active)

  (attr! :shirt-size  :shirt-size-type)
  (attr! :enum :enum-type)

  (series! :enum-types :enum-type)
  (attr! :enums :enum-types))

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
        ::osl/output-ids   [:family-name]))
