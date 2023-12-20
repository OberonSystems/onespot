(ns onespot.lacinia-tests
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core :refer :all :as osc]
            [onespot.lacinia :as osl]
            ;;
            [onespot.common :refer :all]
            :reload))

(defn fetch-people
  [])

(defn fetch-person
  [])

(defn dummy-fn
  [])

(defn make-schema1
  []
  {:queries {:fetch-people {:type    [:person]
                            :resolve fetch-people}
             :fetch-person {:type :person
                            :args    {:person-id nil}
                            :resolve fetch-person}}}
  )

(deftest test-end-points-1
  (register-common!)
  (let [schema {:queries   {:fetch-people  {:type    [:person]
                                            :resolve fetch-people}
                            :fetch-person  {:type    :person
                                            :args    {:person-id nil}
                                            :resolve fetch-person}}
                :mutations {:add-person    {:type    :person
                                            :args    {:new-person nil}
                                            :resolve dummy-fn}
                            :modify-person {:type    :person
                                            :args    {:person nil}
                                            :resolve dummy-fn}}}]
    (is (= (osl/input-types schema)
           [:new-person :person :person-id]))

    (is (= (osl/output-types schema)
           [:person]))))
