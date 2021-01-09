(use 'clojure.core)
(require' [clojure.string :as  str])

(defn space_to_underscore [msg]
  (str/replace msg #" " "_")
  );;replaces spaces in msg with _

(defn valid-msg [key msg]
  (if (re-matches #"^[A-Za-z\s_]"msg)
    (> key 1)
    false)
  );;validates if the msg consists of only letters and if the key is valid

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

(defn encryptmsg [msg key]
  (zipmap
    (get-indexes(count msg) key)( vec msg)
    )
  )
(defn encrypt [msg key]
  (str/join
    (for [[position char]
          (sort-by first (encryptmsg msg key))
          ]
      char
      )
    )
  )
(defn zipmap-encrypt [msg key]
  (if (valid-msg key msg)
    (encryptmsg (space_to_underscore msg) key)
    (str "ERROR: invalid input")
    )
  )
(defn decryptmsg [msg key]
  (zipmap
    (sort (get-indexes (count msg) key))
    (vec msg)
    )
  )

(defn decryptindexes [old-index length]
  (mod old-index length)
  );;gets original symbol location


(defn decrypt [msg key]
  (str/join
    (for [[position char]
          (sort-by first

                   (for [[pos char] (decryptmsg msg key)]
                     [(decryptindexes pos (count msg)) char ]
                     )

                   )
          ]
      char
      )
    )
  )


(defn decrypt [msg key]
  (if (valid-msg key msg)
    (decrypt (space_to_underscore msg) key)
    (str "Error: invalid input!")
    )
  )