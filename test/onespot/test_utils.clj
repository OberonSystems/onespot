(ns onespot.test-utils
  (:require [onespot.core :refer [attr! clear! scalar! rec! series!] :as os]
            [onespot.validators :refer [a-set non-blank-string]]
            [onespot.lacinia  :as lc]
            [onespot.json     :as js]
            [onespot.html     :as ht]
            [onespot.entities :as oe]
            :reload))

(defn register-scalars!
  []
  (clear!)
  (oe/register-common!)
  (scalar! :string-type1 non-blank-string)
  (scalar! :string-type2 non-blank-string
           :label       "My Label For String Type 2"
           :description "A String Type 2")

  (oe/make-enum! :size-enum [{:value :sm :description "Small"}
                             {:value :md :description "Medium"}
                             {:value :lg :description "Large"}
                             {:value :xl :description "Extra Large"}])
  (oe/make-enum! :contact-type-enum [{:value :mobile}
                                     {:value :email}
                                     {:value :whatsapp}]))

(defn register-attrs!
  []
  (register-scalars!)
  (attr! :person-id   ::os/positive-integer)
  (attr! :given-name  ::os/string
         ::js/entity-id :theGivenName
         ::ht/entity-id :the-ht-given-name)
  (attr! :nickname    ::os/string)
  (attr! :family-name ::os/string
         :label "The Family Name")
  ;;
  (attr! :day ::os/local-date)
  (attr! :dob ::os/local-date)
  (attr! :now ::os/instant)
  ;;
  (attr! :contact-type  :contact-type-enum)
  (attr! :contact-value ::os/string)

  (attr! :street-no ::os/string)
  (attr! :street ::os/string)

  (attr! :active? ::os/boolean
         ;; FIXME: the lc entity should fall back to js mapping
         ::js/entity-id :isActive
         ::lc/entity-id :isActive
         ::ht/entity-id :is-active)

  (attr! :size :size-enum)

  (series! :s/sizes :size-enum)
  (attr! :sizes :s/sizes))

(defn register-all!
  []
  (register-attrs!)

  (series! :some-strings :string-type1)
  (series! :tags :string-type1 :validator a-set)

  (rec! :person
        [:person-id
         :given-name
         :family-name
         :size
         :dob
         :active?]
        :identity-ids [:person-id]
        :optional-ids [:dob])


  (rec! :new-person
        (os/rec-value-ids :person))

  (series! :people :person)

  (attr! :manager :person)
  (attr! :managers :people)

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
        ::lc/info {:description "Lacinia Description"})
  
  (rec! ::address
        [:street-no
         :street])
  (attr! :address ::address)
  
  (rec! ::person-with-address
        [:person-id
         :given-name
         :address]
        :identity-ids [:person-id])
  
  (series! ::people-with-addresses ::person-with-address)
  (attr! :people-with-addresses ::people-with-addresses))
