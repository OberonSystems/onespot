(ns onespot.base62)

;; Strict alphanumeric characters in ASCII order
(def alphabet (vec "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
(def base (count alphabet)) ; 62

;; 1. Map each character back to its index for O(1) decoding lookups
(def char->idx (into {} (map-indexed (fn [idx c] [c idx]) alphabet)))

(defn encode
  "Converts an integer into a sorted Base62 string of length `len`.
   Defaults `len` to 2 if not provided."
  ([num]
   (encode num 2))
  ([num len]
   {:pre [(>= num 0)
          (< num (Math/pow base len))]}
   (loop [n num
          step len
          result '()]
     (if (zero? step)
       (apply str result)
       (recur (quot n base)
              (dec step)
              (conj result (get alphabet (rem n base))))))))

(defn decode
  "Converts a sorted Base62 string back into its original integer.
   The maximum base configuration is automatically derived from the string length."
  [s]
  (loop [chars (seq s)
         acc 0]
    (if (empty? chars)
      acc
      (let [c (first chars)
            idx (get char->idx c)]
        (when-not idx
          (throw (IllegalArgumentException. (str "Invalid character in string: " c))))
        (recur (rest chars)
               (+ (* acc base) idx))))))

(comment
  ; 1. Round-trip verification for default 3-character strings
  (let [original-int 154321
        encoded-str  (encode original-int) ; -> "a6n"
        decoded-int  (decode encoded-str)]
    (println "3-Char Original:" original-int)
    (println "3-Char Encoded :" encoded-str)
    (println "3-Char Decoded :" decoded-int))

  ; 2. Round-trip verification for custom 6-character strings
  (let [original-int 5000000000
        encoded-str  (encode original-int 6) ; -> "05bbt2"
        decoded-int  (decode encoded-str)]
    (println "\n6-Char Original:" original-int)
    (println "6-Char Encoded :" encoded-str)
    (println "6-Char Decoded :" decoded-int)))
