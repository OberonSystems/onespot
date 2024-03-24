(ns onespot.entities
  (:require [onespot.core       :as os]
            [onespot.validators :as ov]
            [onespot.lacinia    :as lc]
            [onespot.postgres   :as pg]))

(defn register-common!
  []
  (os/scalar! ::os/boolean ov/true-or-false
              ::lc/info    {:type :boolean})

  (os/scalar! ::os/keyword ov/global-keyword
              ::lc/info    {:type :string})

  (os/scalar! ::os/local-date ov/local-date
              ::lc/info       {:type :string})

  (os/scalar! ::os/instant ov/instant
              ::lc/info    {:type :string})

  (os/scalar! ::os/string  ov/non-blank-string
              :description "Non Blank String"
              ::lc/info    {:type :string})

  (os/scalar! ::os/upper-string ov/non-blank-upper-string
              :description      "Non Blank Uppercase String"
              ::lc/info         {:type :string})

  (os/scalar! ::os/lower-string ov/non-blank-lower-string
              :description      "Non Blank Lowercase String"
              ::lc/info         {:type :string})

  (os/scalar! ::os/svg      ov/non-blank-svg-string?
              :description  "An SVG String"
              ::lc/info     {:type :string})

  (os/scalar! ::os/url-slug ov/upper-url-slug
              :description  "An upper case string that contains only numbers, characters, underscores and hyphens."
              ::lc/info     {:type :string})

  (os/scalar! ::os/alpha-numeric ov/non-blank-string?
              :description       "Non Blank Alpha Numeric String - no white space, 0-9, a-z and A-Z only."
              ::lc/info          {:type :string})

  (os/scalar! ::os/non-negative-integer ov/non-negative-integer
              ::lc/info                 {:type :int})

  (os/scalar! ::os/positive-integer ov/positive-integer
              ::lc/info             {:type :int})

  (os/scalar! ::os/email   ov/email
              :description "A lower case email address."
              ::lc/info    {:type :string})

  (os/scalar! ::os/e164    ov/e164
              :description "A phone no in E164 format, ie +61111222333."
              ::lc/info    {:type :string})

  (os/scalar! ::os/postcode ov/postcode
              :description  "A 4 digit string, 3 digit postcodes should be prefixed by a '0', eg '0800' not '800' for some NT postcodes."
              ::lc/info     {:type :string})

  (os/scalar! ::os/year        ov/year
              ::os/description "A 4 digit string."
              ::lc/info        {:type :string})

  (os/scalar! ::os/ltree       ov/non-blank-string
              :description     "An LTree path, ie `Some.Branch.With_Nodes.And_A_Leaf`."
              ;;
              ::lc/info {:type :string}
              ::pg/info {:type ::pg/ltree})

  (os/scalar! ::os/edn-map ov/edn-map))
