(ns onespot.validate
  (:require [oberon.utils :refer [nil-when->]]
            [onespot.core :as osc]))

;;;

(def ^:dynamic *feedback*)

(defn add-feedback!
  [feedback]
  (if (map? feedback)
    (swap! *feedback* conj feedback)
    (swap! *feedback* concat feedback))
  feedback)

;;;

(defn nil-check
  [{::osc/keys [entity-id] :as entity} value path]
  (when (nil? value)
    (add-feedback! {:path     path
                    :feedback {:entity-id entity-id
                               :code      :missing-value
                               :message   "Value cannot be nil."}})))

(defn empty-check
  [{::osc/keys [entity-id] :as entity} value path]
  (when (empty? value)
    (add-feedback! {:path     path
                    :feedback {:entity-id entity-id
                               :code      :empty-value
                               :message   "Value cannot be empty."}})))

(defn attr-check
  [{::osc/keys [entity-id] :as entity} value path]
  (when-not (contains? value entity-id)
    (add-feedback! {:path     path
                    :feedback {:entity-id entity-id
                               :code      :missing-attr
                               :message   (format "Value must contain a `%s` attr." entity-id)
                               :value     value}})))

(defn validator-check
  [{::osc/keys [entity-id validator] :as entity} value path]
  (when-let [feedback (and validator (validator value))]
    (add-feedback! {:path     path
                    :feedback (merge {:entity-id entity-id
                                      :validator validator}
                                     feedback)})))

(defn validate-entity
  "Performs checks on any sort of entity.

  There are three checks;

  - nil-check: is the value nil and the required?

  - attr-check: does the key exist for the attribute in the hashmap?

    Similiar to the nil-check but gives more detail of the error.

  - validator-check: runs the validator function associated with the
    entity.

  Recurses on `rec` and `series` entities."
  [{::osc/keys [entity-id kind] :as entity}
   value
   path]
  (case kind
    ;; Scalars are straight forward, we validate the value and
    ;; return it at the current path.
    ::osc/scalar (or (nil-check       entity value path)
                     (validator-check entity value path))
    ;;
    ;; Attr's are similiar to scalars but they are named so the path
    ;; now includes their name.
    ;;
    ;; We also validate the content based on either an attr specific
    ;; validator or on the one that is provided with their attr-kind-id
    ;; kind (a scalar or an entity).
    ::osc/attr   (let [attr-entity (osc/attr-type entity)
                       value       (get value entity-id)
                       path        (conj path entity-id)]
                   (validate-entity attr-entity value path))
    ;;
    ;; For records, first check all of the attributes, then call the
    ;; validator on the record itself when all of the attributes
    ;; passed.
    ::osc/rec (or (nil-check   entity value path)
                  (empty-check entity value path)
                  (->> (osc/rec-attrs entity)
                       (map (fn [{attr-id ::osc/entity-id :as attr}]
                              (let [attr-value (get value attr-id)
                                    attr-path  (conj path attr-id)
                                    required?  (osc/required? entity attr)]
                                ;; The attr/entity-id must always be in the map
                                (or (attr-check attr value path)
                                    ;; Required attrs have the
                                    ;; presence checks here, this
                                    ;; avoids recursion on nested maps
                                    ;; and provides a single error for
                                    ;; a missing map rather than a
                                    ;; message for each attribute in
                                    ;; the map.
                                    (and required?
                                         (nil? attr-value)
                                         (nil-check attr nil attr-path))
                                    (and required?
                                         (coll?  attr-value)
                                         (empty? attr-value)
                                         (empty-check attr nil attr-path))
                                    ;;
                                    ;; Finally recurse to validate the
                                    ;; attribute if it has a value, if
                                    ;; not we can short cut the
                                    ;; checking.  Do `not nil?` so that
                                    ;; `false` values are passed
                                    ;; through.
                                    (when-not (nil? attr-value)
                                      (validate-entity attr value path))))))
                       (remove nil?)
                       seq)
                  (validator-check entity value path))
    ;;
    ;; Series share similiarities to attrs and recs but we just use
    ;; the referenced kind to validate and we add an index to the
    ;; path.
    ::osc/series (or (nil-check   entity value path)
                     (empty-check entity value path)
                     (let [series-type (osc/series-type entity)]
                       (->> (map (fn [idx value]
                                   (validate-entity series-type value (conj path idx)))
                                 (range)
                                 value)
                            (remove nil?)
                            seq)))))

(defn validate
  [entish value]
  (let [entity (-> entish
                   osc/canonical-entity-id
                   osc/pull)]
    (binding [*feedback* (atom [])]
      (validate-entity entity value [])
      (when-not (empty? @*feedback*)
        @*feedback*))))
