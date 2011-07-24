(ns shogi.algebraic-notation
  (use [shogi.core] :reload)
  (:use clojure.contrib.str-utils))

                                        ; ▲7六歩

 (let [[a b c] (re-matches #"([-+]?[0-9]+)/([0-9]+)" "22/7")]
   [a b c])

(let [[_ player col row piece]
      (re-matches #"(.)(.)(.)(.)" "▲7六歩")]
  [player col row piece])

(defn kansuji->int [kansuji]
  (case kansuji
        "一" 1 "二" 2 "三" 3
        "四" 4 "五" 5 "六" 6
        "七" 7 "八" 8 "九" 9))
