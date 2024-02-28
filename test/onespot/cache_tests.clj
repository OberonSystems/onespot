(ns onespot.core-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.cache :as c]
            [onespot.core  :as osc]
            [onespot.common :refer :all]
            :reload))

(deftest test-cache-1
  (osc/clear!)

  (is (c/cache-empty?))
  (register-scalars!)
  (is (c/cache-empty?))

  (is (= (c/push :just-my-stuff :test) :test))
  (is (= (c/pull :just-my-stuff)       :test))

  (register-scalars!)
  (is (c/cache-empty?))
  (= (c/pull :just-my-stuff (fn [] :test-this)) :test-this)
  (register-scalars!)
  (is (c/cache-empty?)))
