(ns onespot.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset? intersection union difference]]
            ;;
            [oberon.utils :refer [nil-when->]]
            ;;
            ;; FIXME: Move these to oberon utils
            [onespot.utils :refer [keyword->label ns-keyword->keyword]]))

;;;

(defonce +registry+ (atom {}))

(defn clear!
  []
  (swap! +registry+ (constantly {})))

(defn registered?
  [entity-id]
  (contains? @+registry+ entity-id))

;; Probably include :bag in the future for having an unordered set of
;; different 'kinds' of objects.
(def +kinds+ #{::scalar ::rec ::series ::attr})

(declare kind)
(defn kind?
  [entity-id x]
  (when-not (+kinds+ x)
    (throw (ex-info (format "%s is not a valid ::kind" x) {:entity-id entity-id :x x})))
  (= (kind entity-id) x))

(defn scalar?
  [entity-id]
  (kind? entity-id ::scalar))

(defn rec?
  [entity-id]
  (kind? entity-id ::rec))

(defn series?
  [entity-id]
  (kind? entity-id ::series))

;;;

(defn attr?
  [entity-id]
  (kind? entity-id ::attr))

;;;

(defn push
  [entity-id kind m]
  (when-not (+kinds+ kind)
    (throw (ex-info (format "Cannot register unknown kind: `%s`." kind) {:kind kind})))
  (swap! +registry+ #(assoc % entity-id (assoc m ::kind kind)))
  entity-id)

(defn pull
  ([entity-id]
   (or (get @+registry+ entity-id)
       (throw (ex-info (format "Cannot find entry for `%s` as it is not registered." entity-id)
                       {:entity-id entity-id}))))

  ([entity-id expected-kind]
   (let [{::keys [kind] :as result} (pull entity-id)]
     (when-not (= kind expected-kind)
       (throw (ex-info (format "Pulled entity `%s` does not have the expected-kind: `%s` != `%s`." entity-id kind expected-kind)
                       {:entity-id entity-id :kind kind :expected-kind expected-kind})))
     result)))

;;; --------------------------------------------------------------------------------

(defn canonical-entity-id
  "Takes either a keyword or map.

  If it's a map then it returns the entity-id in the map,
  if not then it returns the keyword"
  [entish]
  (cond
    (and (keyword? entish) (registered? entish))
    entish
    ;;
    (and (map? entish) (-> entish ::entity-id registered?))
    (-> entish ::entity-id)
    ;;
    :else (throw (ex-info (format "Can't find entity-id for `%s`, it must be a map or a keyword." entish)
                          {:entish entish}))))

;;  Functions to pull the entities out of the registry.

(defn scalar
  [entity-id]
  (-> entity-id canonical-entity-id (pull ::scalar)))

(defn rec
  [entity-id]
  (-> entity-id canonical-entity-id (pull ::rec)))

(defn series
  [entity-id]
  (-> entity-id canonical-entity-id (pull ::series)))

(defn attr
  [entity-id]
  (-> entity-id canonical-entity-id (pull ::attr)))

;;  Functions to pull common values out of the registry.

(defn entity-id
  [entity-id]
  (-> entity-id canonical-entity-id pull ::entity-id))

(defn kind
  [entity-id]
  (-> entity-id canonical-entity-id pull ::kind))

(defn description
  [entity-id]
  (-> entity-id canonical-entity-id pull ::description))

(defn validator
  [entity-id]
  (-> entity-id canonical-entity-id pull ::validator))

(defn label
  [entity-id]
  (when-not (registered? entity-id)
    (throw (ex-info (format "Can't find/compute a label for an unregistered entity: `%s`." entity-id))))
  (or (-> entity-id canonical-entity-id pull ::label)
      (keyword->label entity-id)))

;;; --------------------------------------------------------------------------------

(defonce ^:dynamic *warn-on-register?* false)

(defn throw-when-registered
  [kind entity-id]
  (when (and *warn-on-register?* (exists? entity-id))
    (throw (ex-info (format "Entity `%s` has already been registered." entity-id)
                    {:kind kind :entity-id entity-id}))))

;;;

(defn scalar!
  [entity-id validator & {:as info}]
  (throw-when-registered ::scalar entity-id)
  (push entity-id ::scalar (assoc info
                                  ::entity-id entity-id
                                  ::validator validator)))

;;;

(defn attr!
  [entity-id type-id & {:as info}]
  (throw-when-registered ::attr entity-id)
  (when-not (or (scalar? type-id) (rec? type-id) (series? type-id))
    (throw (ex-info (format "Attr `%s` must be associated with a scalar, rec or series not: `%s`." entity-id type-id)
                    {:entity-id entity-id :type-id type-id})))
  (push entity-id ::attr (assoc info
                                ::entity-id entity-id
                                ::type-id   type-id)))

(defn attr-type-id
  [entity-id]
  (-> (attr entity-id) ::type-id))

(defn attr-type
  [entity-id]
  (-> entity-id attr-type-id pull))

;;;

(defn series!
  [entity-id type-id & {:as info}]
  (throw-when-registered ::series entity-id)
  (when-not (or (scalar? type-id) (rec? type-id))
    (throw (ex-info (format "Series `%s` must refer to a scalar or a rec not: `%s`." entity-id type-id)
                    {:entity-id entity-id :type-id type-id})))
  (push entity-id ::attr (assoc info
                                ::entity-id entity-id
                                ::type-id   type-id)))

(defn series-type-id
  [entity-id]
  (-> entity-id series ::type-id))

(defn series-type
  [entity-id]
  (-> entity-id series-type-id pull))

;;;

(defn rec!
  [entity-id attr-ids & {::keys [identity-ids optional-ids]
                         :as info}]
  (throw-when-registered ::rec entity-id)
  (let [attr-ids     (some-> attr-ids     seq vec)
        identity-ids (some-> identity-ids seq vec)
        optional-ids (some-> optional-ids seq vec)
        ;;
        attr-set     (set attr-ids)
        identity-set (set identity-ids)
        optional-set (set optional-ids)
        ;;
        value-ids (some->> attr-ids (remove identity-set) seq vec)]

    (when (empty? attr-ids)
      (throw (ex-info (format "Cannot register rec `%s` without attributes." entity-id)
                      {:entity-id entity-id})))

    (when-not (every? attr? attr-ids)
      (let [unregistered (->> attr-ids (remove attr?) set)]
        (throw (ex-info (format "Cannot register record `%s` with unregistered attributes: %s." entity-id unregistered)
                        {:entity-id entity-id :unregistered unregistered}))))

    (when-not (subset? identity-set attr-set)
      (throw (ex-info (format "Cannot register record `%s` as Identity Keys are not a subset of Attribute Keys." entity-id)
                      {:entity-id entity-id :identity-ids identity-ids :attr-ids attr-ids})))

    (when-not (subset? optional-set attr-set)
      (throw (ex-info (format "Cannot register record `%s` as Optional Keys are not a subset of Attribute Keys." entity-id)
                      {:entity-id entity-id :optional-ids optional-ids :attr-ids attr-ids})))

    (when-let [optional-identity-ids (-> (intersection optional-set identity-set)
                                         (nil-when-> empty?))]
      (throw (ex-info (format "Cannot register record `%s` as some Identity Ids have been marked as optional." entity-id)
                      {:entity-id entity-id :optional-identity-ids optional-identity-ids})))

    (push entity-id ::rec (assoc info
                                 ::entity-id    entity-id
                                 ::attr-ids     attr-ids
                                 ::identity-ids identity-ids
                                 ::value-ids    value-ids
                                 ::optional-set optional-set))))

;;;

(defn rec-attr-ids
  [entity-id]
  (-> entity-id rec ::attr-ids))

(defn rec-attrs
  [entity-id]
  (->> (rec-attr-ids entity-id)
       (map attr)
       seq))

(defn rec-identity-ids
  [entity-id]
  (-> entity-id rec ::identity-ids))

(defn rec-identity-attrs
  [entity-id]
  (->> (rec-identity-ids entity-id)
       (map attr)
       seq))

(defn rec-value-ids
  [entity-id]
  (-> (rec entity-id) ::value-ids))

(defn rec-value-attrs
  [entity-id]
  (->> (rec-value-ids entity-id)
       (map attr)))

(defn rec-optional-set
  [entity-id]
  (-> entity-id rec ::optional-set))

(defn optional?
  [{::keys [optional-set] :as rec}
   {attr-id ::entity-id :as attr}]
  (contains? optional-set attr-id))

(defn required?
  [rec attr]
  (not (optional? rec attr)))

;;; rec helpers that interact with maps.

(defn rec-content
  [entity-id m]
  (-> (select-keys m (rec-attr-ids entity-id))
      (nil-when-> empty?)))

(defn rec-identity
  [entity-id m]
  (-> (select-keys m (rec-identity-ids entity-id))
      (nil-when-> empty?)))

(defn rec-values
  [entity-id m]
  (-> (select-keys m (rec-value-ids entity-id))
      (nil-when-> empty?)))
