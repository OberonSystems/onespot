(ns onespot.common
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.validators :refer :all]
            [onespot.lacinia :as osl]
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
  (scalar! :shirt-size-type  validate-shirt-size
           ;;
           ::osc/enums +shirt-size-enums+)
  )

(defn register-attrs!
  []
  (register-scalars!)
  (attr! :person-id   :positive-integer)
  (attr! :given-name  :string)
  (attr! :nickname    :string)
  (attr! :family-name :string
         ::osc/label "The Family Name")
  ;;
  (attr! :contact-type  :contact-type-enum)
  (attr! :contact-value :string1)

  (attr! :shirt-size  :shirt-size-type)
  (attr! :active?     :boolean
         ::osl/gql-id :is-active))

(defn register-common!
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

  (series! :people :person))
