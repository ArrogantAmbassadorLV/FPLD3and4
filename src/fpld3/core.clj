(ns fpld3.core)
;; 1. Find the last element of a list.
;(def xs (list :a :b :c :d))
(defn my-last [xs]
  (if (= (next xs) nil)
    (first xs)
    (recur (next xs))
    )
  )


;; 2. Find the N-th element of a list.
;(def xs [:a :b :c :d])
(defn get-nth [xs n]
  (if (= n 0)
    (first xs)
    (recur (rest xs) (dec n))
    )
  )


;;3. Find the length of a list
;(def xs [:a :b :c :d])
(def ys '(42 true "hello"))
(defn my-length [xs]
  (if (empty? xs)
    0
    (inc (my-length (next xs)))))

;; 4. Reverse a list.
(defn my-reverse [xs]
(if (not-empty xs)
  (cons (last xs) (my-reverse (butlast xs)))
  ))

;; 5. Find out whether a list is a palindrome. //todo
;;(def xs (list :a :b :c :b :a))
(defn is-palindrome? [xs]
  (or (seq xs)
      (and (= (first xs) (last xs))
           (recur (first (butlast xs)))
           ))
  )

(defn polindromes? [^String s](loop [front 0 back (dec (.length s))]
                                (or (>= front back)
                                    (and (= (.charAt s front) (.charAt s back))
                                         (recur (inc front) (dec back))))))
;; 6. Duplicate the elements of a list.
;;(def xs (list :a :b :c))
(defn duplicate [xs]
  (if (not-empty xs)
    (cons (first xs) (cons (first xs) (duplicate (next xs))))
    ))

;; 7. Eliminate consecutive duplicates of a list.
(def xs (list :a :a :b :b :c :c))
(defn compress [xs]
  (if (and (not-empty xs) (= (first xs) (first (next xs))))
    (compress (next xs))
    (cons (first xs) (compress (next xs)))
    )                                                    ;;if1
  )                                                         ;;defn
;; 8. Remove the N-th element of a list
;(def xs (list :a :b :c :d))
(defn remove-at [xs n]
  (if (= n 0)
            (next xs)
            (cons (first xs) (remove-at (next xs) (dec n)))
            )

  )
;; 9. Insert a new element at the N-th position of a list.//nestrada izsauksana
(defn insert-at [x xs n]
  (if (= n 0)
    (next xs)
    (cons (x) (insert-at (x) (next xs) (dec n)))
    )
  )

;; 10. Create a list containing all integers within a given range.//todo
(defn my-range [a b]

  (if (= a b)
    nil
    (recur (inc a) b)
    ))

;; 11. Concatenate two lists//todo
(defn my-concat [xs ys]
  (when (and (not-empty xs) (not-empty ys))
    ;(cons ys (cons (first xs) (my-concat (next xs)ys)))
    (cons (first xs) (cons (last ys) (my-concat (next xs) (butlast ys))))
    ;;;(cons (first xs) (my-concat (next xs)ys))
    ))

;; 12. Split a list into two parts; the length of the first part is given.
(defn my-drop [xs n]
  (if (= n 1)
    (rest xs)
    (recur (rest xs) (dec n))
    ))
;; 13. Split a list into two parts; the length of the first part is given.
(defn my-drop [xs n]
  (if (= n 1)
    (rest xs)
    (recur (rest xs) (dec n))
    ))

;; 14. Implement the filtering function
(defn my-filter [p xs]
  (when xs
    (if (p (first xs))
      (cons (first xs)(my-filter p (next xs)))
      (my-filter p (next xs))
      )
    ))

;; 15. Implement the mapping function
(defn my-map [f xs]
  (if (not-empty xs)
    (cons (f (first xs)) (my-map f (next xs)))
    ))

;; 16. Implement the reducing function
(defn my-reduce [f acc xs]
  (if (not-empty xs)
    (f (first xs) (my-reduce f acc (next xs)))
    acc))
;; 17. Implement the flattening function//todo
(defn my-flatten [xs]
  nil)

(require '[clojure.string :as  str])

(defn space_to_underscore [msg]
  (str/replace msg #" " "_")
  );;replaces spaces in message with _

(defn valid-msg [key msg]
  (if (re-matches #"^[A-Za-z\s_]+$"msg)
    (> key 1)
    false)
  );;validates if the message consists of only letters and if the key is valid

(defn get-indexes [length key]
  (def row 0)
  (def direction false)
  (vec
    (for [column (range 0 length)]
      (do
        (if (or(= row 0) (= row (- key 1)));direction change
          (def direction (not direction))
          )
        (def matrixIndex (+ column (* row length)))
        (def row
          (if (true? direction)
            (inc row)
            (dec row)
            )
          )
        matrixIndex
        )
      )
    )
  )

(defn encryptmsg [message key]
  (zipmap
    (get-indexes(count message) key)( vec message)
    )
  )
(defn encrypt [message key]
  (str/join
    (for [[position char]
          (sort-by first (encryptmsg message key))
          ]
      char
      )
    )
  )
(defn zipmap-encrypt [message key]
  (if (valid-msg message key)
    (encryptmsg (space_to_underscore message) key)
    (str "ERROR: invalid input")
    )
  )