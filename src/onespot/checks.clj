(ns onespot.checks)

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
  (throw (ex-info "Don't know how to check command."
                  {:command        command
                   :dispatch-value (command-type-dispatcher command)})))
