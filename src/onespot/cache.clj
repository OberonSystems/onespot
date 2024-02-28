(ns onespot.cache)

(def +cache+ (atom {}))

(defn clear!
  []
  (swap! +cache+ (constantly {})))

(defn cache-empty?
  []
  (empty? @+cache+))

(defn push
  [k v]
  (-> (swap! +cache+ assoc k v)
      k))

(defn pull
  [k & [value-fn]]
  (or (get @+cache+ k)
      (when value-fn
        (push k (value-fn)))))
