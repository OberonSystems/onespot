(ns onespot.commands
  (:require [onespot.checks :as ch])
  (:import [java.sql SQLException]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp (atom nil))
(defn- set-tmp!
  [value]
  (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------

(defmulti process-command
  "Processes the command into the database.

  Should take care of any type coercions that aren't handled by default."
  {:arglists '([command])}
  ch/command-type-dispatcher)

(defmethod process-command :default
  [command]
  (throw (ex-info (format "Don't know how to process command: %s." (ch/command-type-dispatcher command))
                  {:command        command
                   :dispatch-value (ch/command-type-dispatcher command)})))

;;; --------------------------------------------------------------------------------

(defn accepted?
  [{:keys [accepted?] :as results}]
  accepted?)

(defn results
  [{:keys [results] :as results}]
  results)

(defn process-commands
  "Process commands in sequence stopping and returning at the first failed one.

  Validates and processes each command in order and strictly one at a
  time.

  This will enable earlier commands to complete and be processed so
  that later ones can validate in an environment that has the results
  of the previously processed commands in place."
  [commands store-command rollback-command]
  (loop [[command & commands] (remove nil? commands)
         results              []]
    (if-not command
      {:accepted? true
       :results   results}
      ;;
      (if-let [errors (ch/check-command command)]
        (do
          (rollback-command)
          {:accepted? false
           :results   (conj results {:command command :errors errors})})
        ;;
        (let [result (try
                       (-> command
                           process-command
                           store-command)
                       (catch Exception ex
                         (or (when (instance? SQLException ex)
                               (.getNextException ex))
                             ex)))]
          (if (instance? Exception result)
            (do
              (rollback-command)
              {:accepted? false
               :results   (conj results {:command   command
                                         :exception result})})
            (recur commands (conj results {:command command
                                           :event   result}))))))))
