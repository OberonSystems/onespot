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

(defn throw-not-implemented
  [f]
  (throw (ex-info (str f " is not implemented yet.") {:handler f})))

(defn resolve-safely
  [resolve]
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

(defn fetch-entity
  [entity-id token & {:keys [matching] :as options}]
  (hs/one entity-id
          (or matching (os/rec-identity entity-id token))
          (assoc options :verbose? true)))

(defn fetch-entities
  [entity-id matching & {:as options}]
  (hs/fetch entity-id
            matching
            (assoc options :verbose? true)))
