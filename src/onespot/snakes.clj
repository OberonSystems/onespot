(ns onespot.snakes
  (:require [clojure.core.memoize :as m]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]))

;;; --------------------------------------------------------------------------------
;;  Memoized as recommended by CSK project.

(def ->camelCase
  (m/fifo csk/->camelCase {} :fifo/threshold 1024))

(def ->camelCaseKeyword
  (m/fifo csk/->camelCaseKeyword {} :fifo/threshold 1024))

(def ->kebab-case-string
  (m/fifo csk/->kebab-case-string {} :fifo/threshold 1024))

(def ->kebab-case-keyword
  (m/fifo csk/->kebab-case-keyword {} :fifo/threshold 1024))

(def ->snake_case_keyword
  (m/fifo csk/->snake_case_keyword {} :fifo/threshold 1024))

(def ->snake_case_string
  (m/fifo csk/->snake_case_string {} :fifo/threshold 1024))

(def ->PascalCaseKeyword
  (m/fifo csk/->PascalCaseKeyword {} :fifo/threshold 1024))

(def ->SCREAMING_SNAKE_CASE_KEYWORD
  (m/fifo csk/->SCREAMING_SNAKE_CASE_KEYWORD {} :fifo/threshold 1024))

(def ->SCREAMING_SNAKE_CASE_STRING
  (m/fifo csk/->SCREAMING_SNAKE_CASE_STRING {} :fifo/threshold 1024))

(defn keys->camel-case
  [m]
  (cske/transform-keys ->camelCase m))

(defn keys->kebab-case
  [m & {:keys [rename-map]}]
  (cske/transform-keys (fn [k]
                         (-> (or (get rename-map k) k)
                             ->kebab-case-keyword))
                       m))

(defn keys->camel-case
  [m & {:keys [rename-map]}]
  (cske/transform-keys (fn [k]
                         (-> (or (get rename-map k) k)
                             ->camelCaseKeyword))
                       m))

(defn keys->snake-case-strings
  [m & {:keys [rename-map]}]
  (cske/transform-keys (fn [k]
                         (-> (or (get rename-map k) k)
                             ->snake_case_string))
                       m))
