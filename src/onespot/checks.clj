(ns onespot.checks
  (:require [clojure.string :as s]
            [onespot.core :as os]
            [onespot.honeysql :refer [entity-absent? entity-present?]]))

(defn format-entity-identity
  [entity-id entity]
  (str entity-id "{"
       (->> (os/rec-identity-ids entity-id)
            (map (fn [k] (str k ":" (-> (get entity k) #_(coerce :string)))))
            (s/join " "))
       "}"))

(defn check-present
  [entity-id record]
  (when (entity-absent? entity-id record)
    (format "%s %s must be present."
            entity-id (format-entity-identity entity-id record))))

(defn check-absent
  [entity-id record]
  (when (entity-present? entity-id record)
    (format "%s %s must be absent."
            entity-id (format-entity-identity entity-id record))))

;;;

(defn command-type-dispatcher
  [{:keys [command-type] :as command}]
  command-type)

(defmulti check-command
  "Check the command prior to processing.

  Should check that values are internally consistent, ie, values fall
  with accepted ranges depending on other values, etc.

  Should check that values are externally consistent, ie, check that
  an existing record exists before trying to modify it."
  {:arglists '([command])}
  command-type-dispatcher)

(defmethod check-command :default
  [command]
  (throw (ex-info (format "Don't know how to check command: %s." (command-type-dispatcher command))
                  {:command        command
                   :dispatch-value (command-type-dispatcher command)})))
