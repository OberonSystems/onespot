(ns onespot.helpers
  (:require [onespot.core       :as os]
            [onespot.validators :as ov]
            [onespot.postgres   :as db]))


(defn make-enum!
  [entity-id enums & {:keys [enum-type validator] :as options}]
  (let [enums (os/canonicalise-enums enums)]
    (os/scalar! entity-id (or validator (ov/make-enum-one-of enums))
                {:enums    enums
                 ::db/info {:type      ::db/enum
                            :enum-type enum-type}})))

(defn make-token!
  [token-id & [parent-id]]
  (let [[token-id parent-id] (if parent-id
                               [token-id parent-id]
                               [(-> (str (name token-id) "-token") keyword)
                                token-id])
        ;;
        entity-ids (os/rec-identity-ids parent-id)]
    (os/rec! token-id entity-ids
             :identity-ids entity-ids
             ;;
             ::db/info (-> (os/rec parent-id) ::db/info))))

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
           ::db/info (::db/info options)
           ;; Remove these from options as we've included them above
           ;; and we don't want those values overridden by anything
           ;; left in options.
           (dissoc options :identity-ids :readonly-ids :optional-ids)))
