(ns onespot.cache-tests
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.cache :as cc]
            [onespot.core  :as os]
            [onespot.common :refer :all]
            :reload))

(deftest test-cache-1
  (os/clear!)

  (is (cc/cache-empty?))
  (register-scalars!)
  (is (cc/cache-empty?))

  (is (= (cc/push :just-my-stuff :test) :test))
  (is (not (cc/cache-empty?)))
  (is (= (cc/pull :just-my-stuff)       :test))

  (register-scalars!)
  (is (cc/cache-empty?))
  (= (cc/pull :just-my-stuff (fn [] :test-this)) :test-this)
  (register-scalars!)
  (is (cc/cache-empty?)))

(deftest test-cache-2
  (cc/clear!)
  (let [c (atom 0)]
    (cc/pull :just-my-stuff (fn []
                              (swap! c inc)
                              :test-this))
    (is (= @c 1))

    (cc/pull :just-my-stuff (fn []
                              (swap! c inc)
                              :test-this))
    (is (= @c 1))
    (cc/pull :just-my-stuff (fn []
                              (swap! c inc)
                              :test-this))
    (is (= @c 1))

    (cc/clear!)))
