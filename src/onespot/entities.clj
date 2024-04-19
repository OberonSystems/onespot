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

(defn register-date-ranges!
  []
  (os/attr! :date-from ::os/local-date)
  (os/attr! :date-to   ::os/local-date)

  (os/rec!  ::os/date-range [:date-from :date-to]
            ;;
            :validator (vl/make-date-range-validator :from? true :to? true)
            ::pg/info  {:type ::pg/date-range})

  (os/rec!  ::os/date-range-from [:date-from :date-to]
            ;;
            :validator (vl/make-date-range-validator :from? true :to? false)
            ::pg/info  {:type ::pg/date-range})

  (os/rec!  ::os/date-range-to [:date-from :date-to]
            ;;
            :validator (vl/make-date-range-validator :from? false :to? true)
            ::pg/info  {:type ::pg/date-range}))

(defn make-enum!
  [entity-id enums & {:keys [enum-type validator] :as options}]
  (let [enums (os/canonicalise-enums enums)]
    (os/scalar! entity-id (or validator (vl/make-enum-one-of enums))
                {:enums    enums
                 ::pg/info {:type      ::pg/enum
                            :enum-type enum-type}})))

(defn make-token!
  [token-id & [parent-id]]
  (let [[token-id parent-id] (if parent-id
                               [token-id parent-id]
                               [(keyword (namespace token-id)
                                         (str (name token-id) "-token"))
                                token-id])
        ;;
        entity-ids (os/rec-identity-ids parent-id)]
    (os/rec! token-id entity-ids
             :identity-ids entity-ids
             ;;
             ::pg/info (-> (os/rec parent-id) ::pg/info))))

(defn make-info!
  [info-id parent-id & {:keys [readonly-ids optional-ids] :as options}]
  (os/rec! info-id
           (os/rec-attr-ids parent-id)
           :identity-ids (os/rec-identity-ids parent-id)
           :optional-ids (-> (concat (os/rec-optional-ids parent-id)
                                     optional-ids)
                             distinct)
           :readonly-ids (-> (concat (os/rec-readonly-ids parent-id)
                                     readonly-ids)
                             distinct)
           ;;
           ::pg/info (::pg/info options)
           ;; Remove these from options as we've included them above
           ;; and we don't want those values overridden by anything
           ;; left in options.
           (dissoc options :identity-ids :readonly-ids :optional-ids)))
