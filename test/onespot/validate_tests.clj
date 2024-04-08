(ns onespot.validate-tests
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require [onespot.core       :as os]
            [onespot.validators :as vs]
            [onespot.validate   :as vl]
            [onespot.common     :refer [register-all!]]
            :reload))

(defn sf
  "Stringify function"
  [f]
  (pr-str f))

(defn sfify-validators
  [messages]
  (mapv (fn [message]
          (update-in message [:feedback :validator] sf))
        messages))

(deftest test-validate-recs
  (register-all!)
  ;; [:person-id
  ;;  :given-name
  ;;  :family-name
  ;;  :shirt-size
  ;;  :dob -- optional
  ;;  :active?]
  (is (nil? (vl/validate :person
                         {:person-id   123
                          :given-name  "Jo"
                          :family-name "Blogs"
                          :shirt-size  :xl
                          :dob         nil
                          :active?     true})))

  (is (= (vl/validate :person
                      {:person-id   123
                       :given-name  "Jo"
                       :family-name "Blogs"
                       :shirt-size  :xl
                       :dob         nil
                       :active?     nil})
         [{:path     [:active?]
           :feedback {:entity-id :active?
                      :code      :missing-value
                      :message   "Value cannot be nil."}}]))

  (is (= (-> (vl/validate :person
                          {:person-id   123
                           :given-name  "Jo"
                           :family-name "Blogs"
                           :shirt-size  :xl
                           :dob         nil
                           :active?     :xxx})
             sfify-validators)
         [{:path     [:active?],
           :feedback {:entity-id :onespot.core/boolean
                      :validator (sf onespot.validators/true-or-false)
                      :code      :bad-value
                      :message   "`:xxx` must be true or false."
                      :value     :xxx}}]))

  (is (= (-> (vl/validate :person
                          {:person-id   123
                           :given-name  "Jo"
                           :family-name "Blogs"
                           :shirt-size  :xl
                           :dob         nil
                           :active?     :xxx})
             sfify-validators)
         [{:path       [:active?]
           :feedback   {:entity-id :onespot.core/boolean
                        :validator (sf onespot.validators/true-or-false)
                        :code      :bad-value
                        :message   "`:xxx` must be true or false."
                        :value    :xxx}}])))
