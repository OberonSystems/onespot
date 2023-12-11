(ns onespot.validators
  (:require [clojure.string :as s]
            ;;
            #?@(:cljs
                [[goog.string :as gstring]
                 [goog.string.format]])
            ;;
            #_[clojure.set    :refer [rename-keys]]
            #_[clojure.pprint :refer [pprint]])
  #?(:clj (:import [java.time Instant LocalDate])))

#_
(defn local-date?
  [x]
  (instance? LocalDate x))

#_
(defn instant?
  [x]
  (instance? Instant x))

(defn non-blank-string?
  [x]
  (when-not (and (string? x)
                 (not (s/blank? x)))
    {:code    :bad-value
     :message "Must be a non blank string."}))

(defn non-blank-upper-string?
  [x]
  (when-not (and (non-blank-string? x)
                 (= x (s/upper-case x)))
    {:code    :bad-value
     :message "Must be a non blank, upper case string."}))

(defn non-blank-lower-string?
  [x]
  (when-not (and (non-blank-string? x)
                 (= x (s/lower-case x)))

    {:code    :bad-value
     :message "Must be a non blank, lower case string."}))

#_
(defn non-blank-svg-string?
  [x]
  (-> (and (non-blank-string? x)
           (re-find #"(?i)<svg\p{Zs}+" x)
           (re-find #"(?i)\bxmlns=\"http://www.w3.org/2000/svg" x))
      boolean))

#_
(defn string-tags?
  [x]
  (and (set? x)
       (every? non-blank-string? x)
       (< 0 (count x))))

(defn positive-integer?
  [x]
  (when-not (and (integer? x) (< 0 x))
    {:code    :bad-value
     :message "Must be a positive integer."}))

#_
(defn edn-map?
  [x]
  ;; FIXME: see if we can check for a `readable` map, ie references to
  ;; functions can't be read, if they are in the map then it's not an
  ;; ednable map.
  (map? x))

#_
(defn postcode?
  [x]
  (and (string? x)
       (re-matches #"^[0-9]{4}$" x)))

#_
(def +email-re+ (-> (str "(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
                         "(?:\\.[a-z0-9!#$%&'*+/=?" "^_`{|}~-]+)*"
                         "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+"
                         "[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")
                    re-pattern))
#_
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

#_
(def +e164-re+ #"^\+[1-9]\d{10,14}$")
#_
(defn e164?
  [x]
  (and (non-blank-string? x)
       (-> (re-matches +e164-re+ x)
           boolean)))
#_
(defn year?
  [x]
  (and (non-blank-string? x)
       (-> (re-matches #"^[0-9]{4}$" x)
           boolean)))
