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
    ::osc/rec (or (->> (osc/rec-attrs entity)
                       (map (fn [{attr-id ::osc/entity-id :as attr}]
                              (let [attr-value (get value attr-id)]
                                ;; The attr/entity-id must always be in the map
                                (or (attr-check attr value path)
                                    ;; Required attrs have the nil check here, this avoids
                                    ;; recursion on nested maps and provides a single error
                                    ;; for a missing map rather than a messeage for each
                                    ;; attribute of the map.
                                    (and (nil? attr-value)
                                         (osc/required? entity attr)
                                         (nil-check attr nil (conj path attr-id)))
                                    ;;
                                    ;; Finally recurse to validate the
                                    ;; attribute if it has a value, if
                                    ;; not we can short cut the
                                    ;; checking.
                                    (when attr-value
                                      (validate-entity attr value path))))))
                       (remove nil?)
                       seq)
                  (validator-check entity value path))
    ;;
    ;; Series share similiarities to attrs and recs but we just use
    ;; the referenced kind to validate and we add an index to the
    ;; path.
    #_(
       ::osc/series (let [child-entity (osc/pull series-kind)
                          errors       (->> (get value id)
                                            (map (fn [idx value]
                                                   ;; this will add any errors to *feedback*
                                                   (validate-entity child-entity value (conj this-path idx)))
                                                 (range))
                                            nil?
                                            seq)
                          error  (when-not errors
                                   (validator-check entity value this-path))]
                      (add-feedback! error)))))

(defn validate
  [entish value]
  (let [entity (-> entish
                   osc/canonical-entity-id
                   osc/pull)]
    (binding [*feedback* (atom [])]
      (validate-entity entity value [])
      (when-not (empty? @*feedback*)
        @*feedback*))))
