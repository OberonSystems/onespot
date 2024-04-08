(ns onespot.validators
  (:require [clojure.string :as s]
            ;;
            [onespot.core :as os])
  (:import [java.time Instant LocalDate]))

(defn non-blank-string?
  [x]
  (and (string? x)
       (not (s/blank? x))))

(defn non-blank-string
  [x]
  (when-not (non-blank-string? x)
    {:code    :bad-value
     :message "Must be a non blank string."
     :value   x}))

(defn non-blank-upper-string
  [x]
  (when-not (and (non-blank-string? x)
                 (= x (s/upper-case x)))
    {:code    :bad-value
     :message "Must be a non blank, upper case string."
     :value   x}))

(defn non-blank-lower-string
  [x]
  (when-not (and (non-blank-string? x)
                 (= x (s/lower-case x)))
    {:code    :bad-value
     :message "Must be a non blank, lower case string."
     :value   x}))

(defn non-blank-svg-string?
  [x]
  (-> (and (non-blank-string? x)
           (re-find #"(?i)<svg\p{Zs}+" x)
           (re-find #"(?i)\bxmlns=\"http://www.w3.org/2000/svg" x))
      boolean))

(defn non-blank-svg-string
  [x]
  (when-not (non-blank-svg-string? x)
    {:code    :bad-value
     :message "Must be a non blank SVG string."
     :value   x}))

(defn upper-url-slug?
  [x]
  (and (non-blank-string? x)
       (= x (s/upper-case x))
       ;; FIXME: Add checks for no spaces, only characters, underscores and digits.
       ))

(defn upper-url-slug
  [x]
  (when-not (upper-url-slug? x)
    {:code    :bad-value
     :message "Must be an upper case string that contains only numbers, characters, underscores and hyphens."
     :value   x}))

(defn global-keyword?
  [x]
  (and (keyword? x)
       (nil? (namespace x))))

(defn global-keyword
  [x]
  (when-not (global-keyword? x)
    {:code    :bad-value
     :message "Must be a global keyword."
     :value   x}))

#_
(defn string-tags?
  [x]
  (and (set? x)
       (every? non-blank-string? x)
       (< 0 (count x))))

(defn year?
  [x]
  (and (string? x)
       (re-matches #"^[0-9]{4}$" x)))

(defn year
  [x]
  (when-not (year? x)
    {:code    :bad-value
     :message "Must be a 4 digit number."
     :value   x}))

(defn postcode?
  [x]
  (and (string? x)
       (re-matches #"^[0-9]{4}$" x)))

(defn postcode
  [x]
  (when-not (postcode? x)
    {:code    :bad-value
     :message "Must be a valid Australian Postcode."
     :value   x}))

(def +email-re+ (-> (str "(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
                         "(?:\\.[a-z0-9!#$%&'*+/=?" "^_`{|}~-]+)*"
                         "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+"
                         "[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")
                    re-pattern))

(defn email?
  "Returns true if the email address is valid, based on RFC 2822. Email
  addresses containing quotation marks or square brackets are considered
  invalid, as this syntax is not commonly supported in practise. The domain of
  the email address is not checked for validity."
  [x]
  ;; Snarfed from: https://github.com/weavejester/valip/blob/master/src/valip/predicates.clj
  (and (non-blank-string? x)
       (-> (re-matches +email-re+ x)
           boolean)))

(defn email
  [x]
  (when-not (email? x)
    {:code    :bad-value
     :message "Must be a lower case email address."
     :value   x}))


(def +e164-re+ #"^\+[1-9]\d{10,14}$")

(defn e164?
  [x]
  (and (non-blank-string? x)
       (-> (re-matches +e164-re+ x)
           boolean)))

(defn e164
  [x]
  (when-not (e164? x)
    {:code    :bad-value
     :message "Must be a phone number in E164 format."
     :value   x}))

;;;

(defn true-or-false
  [x]
  (when-not (boolean? x)
    {:code    :bad-value
     :message (format "`%s` must be true or false." x)
     :value   x}))

;;;

(defn non-negative-integer?
  [x]
  (and (integer? x) (< -1 x)))

(defn non-negative-integer
  [x]
  (when-not (non-negative-integer? x)
    {:code    :bad-value
     :message "Must be an integer that is zero or greater."
     :value   x}))

(defn positive-integer?
  [x]
  (and (integer? x) (< 0 x)))

(defn positive-integer
  [x]
  (when-not (positive-integer? x)
    {:code    :bad-value
     :message "Must be a positive integer."
     :value   x}))

;;;

(defn local-date?
  [x]
  (instance? LocalDate x))

(defn instant?
  [x]
  (instance? Instant x))

(defn local-date
  [x]
  (when-not (local-date? x)
    {:code    :bad-value
     :message "Must be a local date."
     :value   x}))

(defn instant
  [x]
  (when-not (instant? x)
    {:code    :bad-value
     :message "Must be an instant"
     :value   x}))

(defn make-date-range-validator
  [& {:keys [from? to?]}]
  (fn [{:keys [date-from date-to] :as x}]
    (let [message (cond
                    (when (and from? (nil? date-from))              "The `date-from` is required.")
                    (when (and from? (not (local-date? date-from))) (format "The `date-from: %s` must a local-date." date-from))
                    ;;
                    (when (and to? (nil? date-to))                  "The `to-date` is required.")
                    (when (and to? (not (local-date? date-to)))     (format "The `date-to: %s` must a local-date." date-to))
                    ;;
                    (and date-to date-to (not (< date-from date-to)))
                    (format "The `date-from: %s` must be less than the `date-to: %s`."
                            date-from date-to))]
      (when message
        {:code    :bad-value
         :message message
         :value   x}))))

;;;

(defn one-of
  [x values & {:keys [message-fn]}]
  (when-not (contains? values x)
    {:code    :bad-value
     :message (if message-fn
                (message-fn x values)
                (format "Value `%s` must be one of `%s`" x values))
     :value   x
     :values  values}))

(defn make-enum-one-of
  [enums & {:keys [message-fn]}]
  (let [values (->> (os/canonicalise-enums enums)
                    (map :value)
                    set)]
    #(one-of % values :message-fn message-fn)))

(defn a-set
  [x]
  (when-not (set? x)
    {:code    :bad-type
     :message (format "Value `%s` must be a set not `%s`" x (type x))
     :value   x}))

;;;

(defn edn-map
  [x]
  ;; FIXME: see if we can check for a `readable` map, ie references to
  ;; functions can't be read, if they are in the map then it's not an
  ;; ednable map.
  (when-not (map? x)
    {:code    :bad-type
     :message (format "Value `%s` must be a map not `%s`" x (type x))
     :value   x}))

#_
(defn year?
  [x]
  (and (non-blank-string? x)
       (-> (re-matches #"^[0-9]{4}$" x)
           boolean)))
