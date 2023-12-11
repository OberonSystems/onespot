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
#_(defonce +globals+  (atom {}))

(defn clear!
  []
  (swap! +registry+ (constantly {}))
  #_(swap! +globals+  (constantly {})))

;; Probably include ::bag in the future for having an unordered set of
;; different 'kinds' of objects.
;;
;; ::run is a sequence
(def +kinds+ #{::scalar ::attr ::rec ::series})

(declare kind)
(defn kind?
  [k x]
  (when-not (+kinds+ x)
    (throw (ex-info (format "%s is not a valid ::kind" x) {:k k :x x})))
  (= (kind k) x))

(defn scalar?
  [k]
  (kind? k ::scalar))

(defn attr?
  [k]
  (kind? k ::attr))

(defn rec?
  [k]
  (kind? k ::rec))

(defn series?
  [k]
  (kind? k ::series))

;;;

(defn push
  [k kind m]
  (when-not (qualified-keyword? k)
    (throw (ex-info (format "Cannot register unnamespaced keywords: `%s`." k) {:k k})))
  (when-not (+kinds+ kind)
    (throw (ex-info (format "Cannot register unknown kind: `%s`." kind) {:kind kind})))
  (swap! +registry+ #(assoc % k (assoc m ::kind kind)))
  k)

(defn pull
  ([k]
   (or (get @+registry+ k)
       (throw (ex-info (format "Cannot find entry for `%s`, it is not registered." k)
                       {:k k}))))

  ([k & ks]
   (when-let [v (pull k)]
     (get-in v ks))))

(defn exists?
  [k]
  (contains? @+registry+ k))

;;; --------------------------------------------------------------------------------
;;  Functions that retrieve registered entities based upon the type.
;;
;;  If you call (scalar ::address-record) it'll return nil.
;;
;;  These are convenience functions to help with checking the types of
;;  things that are getting pulled from the registry.

(defn scalar
  [k]
  (when (scalar? k)
    (pull k)))

(defn attr
  [k]
  (when (attr? k)
    (pull k)))

(defn rec
  [k]
  (when (rec? k)
    (pull k)))

(defn series
  [k]
  (when (series? k)
    (pull k)))

;;; --------------------------------------------------------------------------------
;;  Functions to pull related values out of the registry.

(defn kind
  [k]
  (pull k ::kind))

(defn pred
  [k]
  (pull k ::pred))

(defn description
  [k]
  (pull k ::description))

(defn validator
  [k]
  (pull k ::validator))

(defn label
  [k]
  (or (pull k ::label)
      (keyword->label k)))

(defn optional?
  [k]
  (-> (pull k ::optional?) boolean))

(defn attr-kind
  [k]
  (pull k ::attr-kind))

(defn attr-keys
  "For ::recs returns a list of the attribute keys."
  [k]
  (pull k ::attr-ks))

(defn attrs
  [k]
  (map pull (attr-keys k)))

(defn identity-keys
  "For ::recs returns a list of the attribute keys that make up the
  identity of this record."
  [k]
  (pull k ::identity-ks))

(defn value-keys
  "For ::recs returns a list of the attribute keys that make up the
  values of this record, ie, the keys that aren't part of the
  identity."
  [k]
  (pull k ::value-ks))

(defn get-rec-identity
  [k record]
  (-> (select-keys record (identity-keys k))
      (nil-when-> empty?)))

(defn get-rec-values
  [k record]
  (-> (select-keys record (value-keys k))
      (nil-when-> empty?)))

;;; --------------------------------------------------------------------------------

(defonce ^:dynamic *warn-on-register?* false)

(defn throw-when-registered
  [kind k]
  (when (and *warn-on-register?* (exists? k))
    (throw (ex-info "Key has already been registered."
                    {:kind kind
                     :key  k}))))

(defn scalar!
  [k validator & {:as info}]
  (throw-when-registered ::scalar k)
  (push k ::scalar (assoc info
                          ::id        k
                          ::validator validator)))

(defn attr!
  [id k & {:as info}]
  (throw-when-registered ::attr id)
  (when-not (or (scalar? k) (rec? k))
    (throw (ex-info (format "Attr %s must be associated with a registered scalar or record not %s." id k)
                    {:id id :k k})))
  (push id ::attr (assoc info
                         ::id        id
                         ::attr-kind k)))

(defn rec!
  [k attr-ks & {:keys [::identity-ks]
                :as info}]
  (throw-when-registered ::rec k)
  (let [attr-ks     (some-> attr-ks     seq vec)
        identity-ks (some-> identity-ks seq vec)
        ;;
        attr-set     (set attr-ks)
        identity-set (set identity-ks)
        ;;
        value-ks (some->> attr-ks (remove identity-set) seq vec)]

    (when (empty? attr-ks)
      (throw (ex-info (format "Cannot register rec `%s` without attributes." k)
                      {:k k})))

    (when-not (every? attr? attr-ks)
      (let [unregistered (->> attr-ks (remove attr?) set)]
        (throw (ex-info (format "Cannot register record `%s` with unregistered attributes: %s." k unregistered)
                        {:k k :unregistered unregistered}))))

    (when-not (subset? identity-set attr-set)
      (throw (ex-info (format "Cannot register record `%s` as Identity Keys are not a subset of Attribute Keys." k)
                      {:k k :identity-set identity-set :attr-set attr-set})))

    (when (some optional? identity-ks)
      (ex-info (format "Cannot register rec `%s` as identity-ks contains optional attrs." k)
               {:k k
                :optional-attrs (filter optional? identity-ks)}))

    (push k ::rec (assoc info
                         ::id          k
                         ::attr-ks     attr-ks
                         ::identity-ks identity-ks
                         ::value-ks    value-ks))))

;;; --------------------------------------------------------------------------------

#_
(defn ->sp
  "Converts 'anything' into the spectacular equivalent"
  [sp-type v & {:keys [->sp-key]}]
  (let [->])

  (some->> (concat (attribute-keys entity-type)
                   ;; It's handy to be able to also provide a way for
                   ;; our caller to include some extra attributes that
                   ;; they might be interested in.
                   extras)
           (map (fn [ref-type]
                  (let [value (if (contains? m ref-type)
                                (get m ref-type)
                                ;; We need to test on the presence
                                ;; of the key rather than the value
                                ;; as we don't want a nil or false
                                ;; overridden by a non-namespaced
                                ;; variant.
                                ;;
                                ;; If m doesn't contain the
                                ;; namespaced variant then we can go
                                ;; for a plain keyword.
                                ;;
                                ;; This is often useful for getting
                                ;; things from maps returned from
                                ;; databases.
                                (get m (ns-keyword->keyword ref-type)))]
                    ;; We want to let false values through
                    (when-not (nil? value)
                      [ref-type value]))))
           (remove nil?)
           seq
           (into {})))
