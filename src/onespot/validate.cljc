(ns onespot.validate
  (:require [oberon.utils :refer [nil-when->]]
            [onespot.core :as osc]))

;;;

(def ^:dynamic *feedback*)

(defn add-feedback!
  [feedback]
  (if (map? feedback)
    (swap! *feedback* conj feedback)
    (swap! *feedback* concat feedback)))

;;;

(defn nil-check
  [{::osc/keys [id optional?] :as entity} value path]
  (when (and (nil? value) (not optional?))
    {:path path :feedback {:code      :missing
                           :message   "Value cannot be nil for a required entity."
                           :entity-id id
                           :value     value}}))

(defn attr-check
  [{::osc/keys [id] :as entity} value path]
  (when-not (contains? value id)
    {:path path :feedback {:code      :missing
                           :message   (format "Attr: `%s` cannot find itself in hashmap value." id)
                           :entity-id id
                           :value     value}}))

(defn validator-check
  [{::osc/keys [validator] :as entity} value path]
  (when-let [feedback (and validator (validator value))]
    {:path path :feedback feedback}))

(defn validate-entity
  [{::osc/keys [id kind attr-kind series-kind optional?] :as entity}
   value
   path]
  (let [this-path (conj path id)]
    (case kind
      ;; Scalars are straight forward, we validate the value and
      ;; return it at the current path.
      ::osc/scalar (some-> (or (nil-check       entity value path)
                               (validator-check entity value path))
                           add-feedback!)
      ;;
      ;; Attr's are similiar to scalars but they are named so the path
      ;; now includes their name and we also validate the content
      ;; based on either a specific validator or on the one that is
      ;; provided with their referenced kind (a scalar or an entity).
      ::osc/attr   (let [attr-value (get value id)]
                     (some-> (or (attr-check      entity value      path)
                                 (nil-check       entity attr-value path)
                                 (validator-check entity attr-value path)
                                 (when-let [entity (osc/pull attr-kind)]
                                   (validator-check entity attr-value path)))
                             add-feedback!))
      ;;
      ;; For records, first check all of the attributes, then call the
      ;; validator on the record itself if all of the attributes
      ;; passed.
      ::osc/rec    (let [errors (->> (osc/attrs id)
                                     (map (fn [{:osc/keys [id] :as attr}]
                                            ;; this will add any errors to *feedback*
                                            (validate-entity attr value (conj this-path id))))
                                     (remove nil?)
                                     seq)
                         error  (when-not errors
                                  (validator-check entity value this-path))]
                     (add-feedback! error))
      ;;
      ;; Series share similiarities to attrs and recs but we just use
      ;; the referenced kind to validate and we add an index to the
      ;; path.
      ::osc/series (let [child-entity (osc/pull series-kind)
                         errors       (->> (get value id)
                                           (map (fn [idx value]
                                                  (validate-entity child-entity value (conj this-path idx)))
                                                (range))
                                           nil?
                                           seq)
                         error  (when-not errors
                                  (validator-check entity value this-path))]
                     (add-feedback! errors)
                     (add-feedback! error)))))

(defn validate
  [entity value & {:keys [path]
                   :or   {path []}}]
  (let [entity (if (keyword? entity)
                 (osc/pull entity)
                 entity)]
   (binding [*feedback* (atom [])]
     (validate-entity entity value [])
     (when-not (empty? @*feedback*)
       @*feedback*))))
