(ns fpld3.core)
(require' [clojure.string :as  str])

(defn space_to_underscore [msg]
  (str/replace msg #" " "_")
  );;replaces spaces in message with _

(defn valid-msg [key msg]
  (if (re-matches #"^^[A-Za-z\s_]"msg)
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

