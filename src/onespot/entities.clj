(ns onespot.entities
  (:require [onespot.core       :as osc]
            [onespot.validators :as osv]
            [onespot.lacinia    :as osl]
            [onespot.postgres   :as opg]))

(defn register-common!
  []
  (osc/scalar! ::osc/boolean  osv/true-or-false
               ::osl/gql-type :boolean)

  (osc/scalar! ::osc/keyword  osv/global-keyword
               ::osl/gql-type :string
               ::opg/info     #::opg{:type ::opg/keyword})

  (osc/scalar! ::osc/local-date osv/local-date
               ::osl/gql-type   :string)

  (osc/scalar! ::osc/instant  osv/instant
               ::osl/gql-type :string)

  (osc/scalar! ::osc/string      osv/non-blank-string
               ::osc/description "Non Blank String"
               ::osl/gql-type    :string)

  (osc/scalar! ::osc/upper-string osv/non-blank-upper-string
               ::osc/description  "Non Blank Uppercase String"
               ::osl/gql-type     :string)

  (osc/scalar! ::osc/lower-string osv/non-blank-lower-string
               ::osc/description  "Non Blank Lowercase String"
               ::osl/gql-type     :string)

  (osc/scalar! ::osc/svg         osv/non-blank-svg-string?
               ::osc/description "An SVG String"
               ::osl/gql-type    :string)

  (osc/scalar! ::osc/url-slug    osv/upper-url-slug
               ::osc/description "An upper case string that contains only numbers, characters, underscores and hyphens."
               ::osl/gql-type    :string)

  (osc/scalar! ::osc/non-negative-integer osv/non-negative-integer
               ::osl/gql-type             :int)

  (osc/scalar! ::osc/positive-integer osv/positive-integer
               ::osl/gql-type         :int)

  (osc/scalar! ::osc/email       osv/email
               ::osc/description "A lower case email address."
               ::osl/gql-type    :string)

  (osc/scalar! ::osc/e164        osv/e164
               ::osc/description "A phone no in E164 format, ie +61111222333."
               ::osl/gql-type    :string)

  (osc/scalar! ::osc/postcode    osv/postcode
               ::osc/description "A 4 digit string, 3 digit postcodes should be prefixed by a '0', eg '0800' not '800' for some NT postcodes."
               ::osl/gql-type    :string)

  (osc/scalar! ::osc/year        osv/year
               ::osc/description "A 4 digit string."
               ::osl/gql-type    :string))
