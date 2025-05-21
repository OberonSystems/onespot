(ns onespot.lacinia-helpers
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.stacktrace :refer [print-stack-trace]]
            ;;
            [taoensso.timbre :as log]
            ;;
            [com.walmartlabs.lacinia :as ql]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            ;;
            [oberon.utils :refer [dump-> dump->> canonical-values]]
            ;;
            [onespot.core     :as os]
            [onespot.json     :as js]
            [onespot.lacinia  :as lc]
            [onespot.postgres :as pg]
            [onespot.honeysql :as hs]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp      (atom nil))
(defn-   ^:private set-tmp! [value] (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------

(defn parse-json-args
  [schema endpoint-id json]
  (some-> json
          lc/json->lacinia-json
          (lc/args->core schema endpoint-id)))

;;; --------------------------------------------------------------------------------

(defn throw-not-implemented
  [f]
  (throw (ex-info (str f " is not implemented yet.") {:handler f})))

(defn resolve-safely
;;;

(defn canonicalise-args
  [resolve]
  (fn [context args value]
    (resolve context (canonical-values args) value)))

(defn wrap-canonicalise-args
  [{:keys [resolve] :as record} & _]
  ;; We call canonical-values here as args can be passed as JSON from
  ;; a browser, which can have leading/trailing spaces, empty strings
  ;; instead of NULLs for number fields, etc, and all sorts of mess.
  ;; This is the first step to make the JSON conform to a 'standard'
  ;; shape.
  (assoc record :resolve (canonicalise-args resolve)))

  (fn [context args value]
    (try
      ;; We call canonical-values here as args can be passed JSON from
      ;; a browser, which can have leading/trailing spaces, empty
      ;; strings instead of NULL for number fields and all sorts of
      ;; mess.
      (resolve context (canonical-values args) value)
      (catch clojure.lang.ExceptionInfo e
        (let [st (with-out-str (print-stack-trace e))]
          (log/info st)
          (resolve-as nil {:message st
                           :ex-data (ex-data e)}))))))

(defn wrap-resolve-safely
  [{:keys [resolve] :as record} & _]
  (assoc record :resolve (resolve-safely resolve)))

(defn wrap-exporter
  [exporter filter-entity-id]
  (fn [request filter]
    (exporter request
              (->> filter
                   lc/->core-keys
                   (js/->core filter-entity-id)))))

;;;

(defn resolve-with-message
  [message & [errors]]
  (resolve-as nil (merge {:message message}
                         (when errors {:errors errors}))))

(defn resolve-with-error
  [{:keys [accepted? results]}]
  (let [{:keys [command errors exception]} (->> results
                                                (filter #(or (contains? % :errors)
                                                             (contains? % :exception)))
                                                first)
        error (when errors    (with-out-str (pprint errors)))
        st    (when exception (with-out-str (print-stack-trace exception)))]
    (if errors
      (do
        (log/info error)
        (resolve-as nil {:errors errors :message (str "Command " (:command-type command) " Failed.")}))
      (do
        (log/info st)
        (resolve-as nil {:exception st :message (str "Command " (:command-type command) " Failed.")})))))

(defn make-resolvable-error
  "Make an error that can be passed to `resolve-with-error`.

  This can be useful while doing the `checks`."
  [command-type error]
  {:accepted? false
   :results   [{:command {:command-type command-type}
                :errors  (cond
                           (string? error) {:entity error}
                           (map?    error) error
                           :else (throw (ex-info (format "Can't convert %s to a response error" error) {:error error})))}]})

;;; --------------------------------------------------------------------------------

(defn fetch-one
  "Fetches a single entity based upon the matching criteria.

  Throw by default when no matching record found."
  [entity-id matching & {:as options}]
  (hs/one entity-id
          matching
          (merge {:verbose? true
                  :throw?   true}
                 options)))

(defn fetch-entity
  "Fetches a single entity based upon the identity values extracted from the token.

  Throw by default when no matching record found."
  [entity-id token & {:keys [matching] :as options}]
  (hs/one entity-id
          (merge matching (os/rec-identity entity-id token))
          (merge {:verbose? true
                  :throw?   true}
                 (dissoc options :matching))))

(defn fetch-entities
  "Fetches many entities based upon matching, can return nil."
  [entity-id matching & {:as options}]
  (hs/fetch entity-id
            matching
            (assoc options :verbose? true)))
