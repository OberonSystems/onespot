(ns onespot.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [subset? intersection union difference]]
            ;;
            [oberon.utils :refer [nil-when-> hash-map* capitalize-keyword]]
            ;;
            [onespot.cache :as cc]))

;;;

(defonce +registry+ (atom {}))

(defn clear!
  []
  (cc/clear!)
  (swap! +registry+ (constantly {})))

(defn registered?
  [entity-id]
  (contains? @+registry+ entity-id))

;; Probably include :bag in the future for having an unordered set of
;; different 'kinds' of objects.
(def +kinds+ #{:scalar :rec :series :attr})

(declare kind)
(defn kind?
  [entity-id x]
  (when-not (+kinds+ x)
    (throw (ex-info (format "%s is not a valid :kind" x) {:entity-id entity-id :x x})))
  (= (kind entity-id) x))

(defn scalar?
  [entity-id]
  (kind? entity-id :scalar))

(defn enum?
  [entity-id]
  (and (kind? entity-id :scalar)
       (-> @+registry+
           (get entity-id)
           (contains? :enums))))

(defn canonicalise-enums
  "Accepts a sequence of
  - value, description maps or
  - value keywords
  and converts it into a vector maps that contain at least a :value.

  Any additional values contained in the maps are preserved."
  [enums]
  (->> enums
       (mapv (fn [enum]
               (cond
                 (keyword? enum) {:value enum}
                 ;;
                 (and (map? enum) (contains? enum :value)) enum
                 ;;
                 (and (vector? enum)
                      (< 0 (count enum) 3))
                 (hash-map* :value       (first enum)
                            :description (second enum))
                 ;;
                 :else (throw (ex-info (format "Can't canonicalise %s into an enum." enum)
                                       {:enum enum})))))))

(defn rec?
  [entity-id]
  (kind? entity-id :rec))

(defn series?
  [entity-id]
  (kind? entity-id :series))

(defn attr?
  [entity-id]
  (kind? entity-id :attr))

;;;

(defn push
  [entity-id kind m]
  (when-not (+kinds+ kind)
    (throw (ex-info (format "Cannot register unknown kind: `%s`." kind) {:kind kind})))
  (cc/clear!)
  (swap! +registry+ #(assoc % entity-id (assoc m :kind kind)))
  entity-id)

(defn pull
  ([entity-id]
   (or (get @+registry+ entity-id)
       (throw (ex-info (format "Cannot find entry for `%s` as it is not registered." entity-id)
                       {:entity-id entity-id}))))

  ([entity-id expected-kind]
   (let [{:keys [kind] :as result} (pull entity-id)]
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
    (and (map? entish) (-> entish :entity-id registered?))
    (-> entish :entity-id)
    ;;
    :else (throw (ex-info (format "Can't find entity-id for `%s`, it must be a map or a keyword." entish)
                          {:entish entish}))))

;;  Functions to pull the entities out of the registry.

(defn scalar
  [entity-id]
  (-> entity-id canonical-entity-id (pull :scalar)))

(defn rec
  [entity-id]
  (-> entity-id canonical-entity-id (pull :rec)))

(defn series
  [entity-id]
  (-> entity-id canonical-entity-id (pull :series)))

(defn attr
  [entity-id]
  (-> entity-id canonical-entity-id (pull :attr)))

;;  Functions to pull common values out of the registry.

(defn entity-id
  [entity-id]
  (-> entity-id canonical-entity-id pull :entity-id))

(defn kind
  [entity-id]
  (-> entity-id canonical-entity-id pull :kind))

(defn description
  [entity-id]
  (-> entity-id canonical-entity-id pull :description))

(defn validator
  [entity-id]
  (-> entity-id canonical-entity-id pull :validator))

(defn label
  [entity-id]
  (when-not (registered? entity-id)
    (throw (ex-info (format "Can't find/compute a label for an unregistered entity: `%s`." entity-id))))
  (or (-> entity-id canonical-entity-id pull :label)
      (capitalize-keyword entity-id)))

;;; --------------------------------------------------------------------------------

(defonce ^:dynamic *warn-on-register?* false)

(defn throw-when-registered
  [kind entity-id]
  (when (and *warn-on-register?* (registered? entity-id))
    (throw (ex-info (format "Entity `%s` has already been registered." entity-id)
                    {:kind kind :entity-id entity-id}))))

;;;

(defn scalar!
  [entity-id validator & {:as info}]
  (throw-when-registered :scalar entity-id)
  (push entity-id :scalar (assoc info
                                  :entity-id entity-id
                                  :validator validator)))

;;;

(defn attr!
  [entity-id attr-entity-id & {:as info}]
  (throw-when-registered :attr entity-id)
  (when-not (or (scalar? attr-entity-id) (rec? attr-entity-id) (series? attr-entity-id))
    (throw (ex-info (format "Attr `%s` must be associated with a scalar, rec or series not: `%s`." entity-id attr-entity-id)
                    {:entity-id entity-id :attr-entity-id attr-entity-id})))
  (push entity-id :attr (assoc info
                                :entity-id      entity-id
                                :attr-entity-id attr-entity-id)))

(defn attr-entity-id
  [entity-id]
  (-> (attr entity-id) :attr-entity-id))

(defn attr-entity
  [entity-id]
  (-> entity-id attr-entity-id pull))

;;;

(defn series!
  [entity-id series-entity-id & {:as info}]
  (throw-when-registered :series entity-id)
  (when-not (or (scalar? series-entity-id) (rec? series-entity-id))
    (throw (ex-info (format "Series `%s` must refer to a scalar or a rec not: `%s`." entity-id series-entity-id)
                    {:entity-id entity-id :series-entity-id series-entity-id})))
  (push entity-id :series (assoc info
                                  :entity-id        entity-id
                                  :series-entity-id series-entity-id)))

(defn series-entity-id
  [entity-id]
  (-> entity-id series :series-entity-id))

(defn series-entity
  [entity-id]
  (-> entity-id series-entity-id pull))

;;;

(defn rec!
  [entity-id attr-ids &
   {:keys [identity-ids optional-ids readonly-ids
           validator] :as info}]
  (throw-when-registered :rec entity-id)
  (let [attr-ids     (some-> attr-ids     seq vec)
        identity-ids (some-> identity-ids seq vec)
        optional-ids (some-> optional-ids seq vec)
        readonly-ids (some-> readonly-ids seq vec)
        ;;
        attr-set     (set attr-ids)
        identity-set (set identity-ids)
        optional-set (set optional-ids)
        readonly-set (set readonly-ids)
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
      (throw (ex-info (format "Cannot register record `%s` as identity-ids is not a subset of attr-ids." entity-id)
                      {:entity-id entity-id :identity-ids identity-ids :attr-ids attr-ids})))

    (when-not (subset? optional-set (union attr-set readonly-set))
      (throw (ex-info (format "Cannot register record `%s` as optional-ids is not a subset of attr-ids and readonly-ids." entity-id)
                      {:entity-id    entity-id
                       :attr-ids     attr-ids
                       :readonly-ids readonly-ids
                       :optional-ids optional-ids
                       :unregistered (difference optional-set attr-set readonly-set)})))

    (when-let [optional-identity-ids (-> (intersection optional-set identity-set)
                                         (nil-when-> empty?))]
      (throw (ex-info (format "Cannot register record `%s` as some identity-ids have been marked as optional." entity-id)
                      {:entity-id entity-id :optional-identity-ids optional-identity-ids})))

    ;; Readonly Checks
    (when-not (every? attr? readonly-ids)
      (let [unregistered (->> readonly-ids (remove attr?) set)]
        (throw (ex-info (format "Cannot register record `%s` with unregistered readonly-ids: %s." entity-id unregistered)
                        {:entity-id entity-id :unregistered unregistered}))))

    (when-let [readonly-identity-ids (-> (intersection readonly-set attr-set)
                                         (nil-when-> empty?))]
      (throw (ex-info (format "Cannot register record `%s` as some readonly-ids have been included in the attr-ids." entity-id)
                      {:entity-id entity-id :readonly-identity-ids readonly-identity-ids})))

    (push entity-id :rec (assoc info
                                 :entity-id    entity-id
                                 :attr-ids     attr-ids
                                 :identity-ids identity-ids
                                 :value-ids    value-ids
                                 :optional-set (-> optional-set (nil-when-> empty?))
                                 :readonly-set (-> readonly-set (nil-when-> empty?))
                                 :validator    validator))))

;;;

(defn attrs
  []
  (->> @+registry+
       (filter #(-> % first attr?))
       (map second)))

(defn recs
  []
  (->> @+registry+
       (filter #(-> % first rec?))
       (map second)))

(defn make-core-key->ns-key
  [ns-entity-id]
  (->> (attrs)
       (map (fn [attr]
              (let [entity-id    (entity-id    attr)
                    ns-entity-id (ns-entity-id attr)]
                (when (and entity-id
                           ns-entity-id
                           (not= entity-id
                                 ns-entity-id))
                  [entity-id ns-entity-id]))))
       (into {})))

(defn make-ns-key->core-key
  [ns-entity-id]
  (->> (attrs)
       (map (fn [attr]
              (let [entity-id    (entity-id    attr)
                    ns-entity-id (ns-entity-id attr)]
                (when (and entity-id
                           ns-entity-id
                           (not= entity-id
                                 ns-entity-id))
                  [ns-entity-id entity-id]))))
       (into {})))

;;;

(defn rec-readonly-ids  [entity-id]
  (-> entity-id rec :readonly-ids))

(defn rec-readonly-attrs
  [entity-id]
  (->> (rec-readonly-ids entity-id)
       (map attr)
       seq))

(defn rec-attr-ids
  [entity-id & {:keys [readonly?]}]
  (into (-> entity-id rec :attr-ids)
        (when readonly? (rec-readonly-ids entity-id))))

(defn rec-attrs
  [entity-id & {:keys [readonly?]}]
  (->> (rec-attr-ids entity-id :readonly? readonly?)
       (map attr)
       seq))

(defn rec-identity-ids
  [entity-id]
  (-> entity-id rec :identity-ids))

(defn rec-optional-ids
  [entity-id]
  (-> entity-id rec :optional-ids))

(defn rec-identity-attrs
  [entity-id]
  (->> (rec-identity-ids entity-id)
       (map attr)
       seq))

(defn rec-value-ids
  [entity-id]
  (-> (rec entity-id) :value-ids))

(defn rec-value-attrs
  [entity-id]
  (->> (rec-value-ids entity-id)
       (map attr)))

(defn rec-optional-set
  [entity-id]
  (-> entity-id rec :optional-set))

(defn optional?
  [{:keys [optional-set] :as rec}
   {attr-id :entity-id :as attr}]
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
  [entity-id m & {:keys [throw?] :or {throw? true}}]
  (let [identity-ids (rec-identity-ids entity-id)
        result       (-> (select-keys m identity-ids)
                         (nil-when-> empty?))]
    (when (and throw? (some #(-> (get result %) nil?)
                            identity-ids))
      (throw (ex-info (format "Failed to extract identity for %s, missing values." entity-id)
                      {:entity-id    entity-id
                       :identity-ids identity-ids
                       :m            m})))
    result))

(defn rec-values
  [entity-id m & {:keys [throw?]}]
  (let [value-ids (rec-value-ids entity-id)
        result    (-> (select-keys m value-ids)
                      (nil-when-> empty?))]
    (when (and throw? value-ids (nil? result))
      (throw (ex-info (format "Failed to extract values for %s" entity-id)
                      {:entity-id entity-id
                       :value-ids value-ids
                       :m         m})))
    result))

;;; --------------------------------------------------------------------------------

(defn walk-entities
  "Walks the entities and visits all referenced entities, returning a
  distinct set.

  Avoids recursion by tracking previously visited entity-ids."
  [entity-id-queue & {:keys [readonly?]}]
  (loop [queue   (if (keyword? entity-id-queue)
                   [entity-id-queue]
                   entity-id-queue)
         visited #{}]
    (let [[entity-id & queue] queue]
      (cond
        (nil? entity-id) visited
        ;;
        (contains? visited entity-id)
        (recur queue visited)
        ;;
        (scalar? entity-id)
        (recur queue (conj visited entity-id))
        ;;
        (rec? entity-id)
        (recur (into queue (rec-attr-ids entity-id :readonly? readonly?))
               (conj visited entity-id))
        ;;
        (series? entity-id)
        (recur (conj queue (series-entity-id entity-id))
               (conj visited entity-id))
        ;;
        (attr? entity-id)
        (recur (conj queue (attr-entity-id entity-id))
               (conj visited entity-id))))))
