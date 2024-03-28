(ns onespot.entities
  (:require [onespot.core       :as os]
            [onespot.validators :as vl]
            [onespot.lacinia    :as lc]
            [onespot.postgres   :as pg]))

(defn register-common!
  []
  (os/scalar! ::os/boolean vl/true-or-false
              ::lc/info    {:type :boolean})

  (os/scalar! ::os/keyword vl/global-keyword
              ::lc/info    {:type :string})

  (os/scalar! ::os/local-date vl/local-date
              ::lc/info       {:type :string})

  (os/scalar! ::os/instant vl/instant
              ::lc/info    {:type :string})

  (os/scalar! ::os/string  vl/non-blank-string
              :description "Non Blank String"
              ::lc/info    {:type :string})

  (os/scalar! ::os/upper-string vl/non-blank-upper-string
              :description      "Non Blank Uppercase String"
              ::lc/info         {:type :string})

  (os/scalar! ::os/lower-string vl/non-blank-lower-string
              :description      "Non Blank Lowercase String"
              ::lc/info         {:type :string})

  (os/scalar! ::os/svg      vl/non-blank-svg-string?
              :description  "An SVG String"
              ::lc/info     {:type :string})

  (os/scalar! ::os/url-slug vl/upper-url-slug
              :description  "An upper case string that contains only numbers, characters, underscores and hyphens."
              ::lc/info     {:type :string})

  (os/scalar! ::os/alpha-numeric vl/non-blank-string?
              :description       "Non Blank Alpha Numeric String - no white space, 0-9, a-z and A-Z only."
              ::lc/info          {:type :string})

  (os/scalar! ::os/non-negative-integer vl/non-negative-integer
              ::lc/info                 {:type :int})

  (os/scalar! ::os/positive-integer vl/positive-integer
              ::lc/info             {:type :int})

  (os/scalar! ::os/email   vl/email
              :description "A lower case email address."
              ::lc/info    {:type :string})

  (os/scalar! ::os/e164    vl/e164
              :description "A phone no in E164 format, ie +61111222333."
              ::lc/info    {:type :string})

  (os/scalar! ::os/postcode vl/postcode
              :description  "A 4 digit string, 3 digit postcodes should be prefixed by a '0', eg '0800' not '800' for some NT postcodes."
              ::lc/info     {:type :string})

  (os/scalar! ::os/year        vl/year
              ::os/description "A 4 digit string."
              ::lc/info        {:type :string})

  (os/scalar! ::os/ltree       vl/non-blank-string
              :description     "An LTree path, ie `Some.Branch.With_Nodes.And_A_Leaf`."
              ;;
              ::lc/info {:type :string}
              ::pg/info {:type ::pg/ltree})

  (os/scalar! ::os/edn-map vl/edn-map))

