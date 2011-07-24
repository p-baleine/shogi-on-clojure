(ns shogi.player
  (:use [shogi.core] :reload)
  (:use clojure.set clojure.contrib.str-utils clojure.contrib.math))

(defn all-valid-moves [p board]
  (let [results
        (filter #(and (not (emp? board %))
                      (= p (player (bref board %))))
                (union (on-board-squares)
                       (captured-squares p)))]
    (apply
     concat
     (map (fn [from]
            (let [valids (valid-moves from board)]
              (when-not (empty? valids)
                (map (fn [to]
                       (if (promotable? p from to board)
                         [from to true]
                         [from to false]))
                     valids))))
          results))))

(defn random-strategy [player board]
  (let [moves (all-valid-moves player board)]
    (nth moves
         (.nextInt (java.util.Random.) (count moves)))))

(alias 'f 'shogi.core.first)

(defn on-board-weights []
  {EMP 0 f/HUU 0 f/KYO 100 f/KEI 500 f/GIN 600 f/KIN 800
   f/OUU 1500 f/HIS 1300 f/KAK 900
   f/TKIN 9999 f/NKYO 1100 f/NKEI 1000 f/NGIN 900
   f/RYUMA 1500 f/RYUOU 1700})

(defn captured-weights []
  {EMP 0 f/HUU 0 f/KYO 120 f/KEI 550 f/GIN 660 f/KIN 880
   f/OUU 999999 f/HIS 1400 f/KAK 1650})

(defn weighted-squares [p board]
  (let [opp (opponent p)]
    (letfn [(sum [weights squares]
              (reduce +
                      (map #(let [piece (bref board %)
                                  w (weights (abs piece))]
                              (if-not (= (player piece) opp) w (- w)))
                           squares)))]
      (+ (sum (on-board-weights) (on-board-squares))
         (sum (captured-weights) (captured-squares p))))))

(defn minimax [player board ply eval-fn]
  (if (= ply 0)
    [(eval-fn player board) nil]
    (let [all-moves (all-valid-moves player board)]
      (if (nil? all-moves)
        Integer/MIN_VALUE
        (apply
         (partial max-key first)
         (for [move all-moves]
           (let [[from to promotion] move
                 [board2 captured] (make-move! from to promotion (ref (vec @board)))]
             (when captured
               (capture-piece! player captured board2))
             (let [[val _] (minimax (opponent player) board2
                               (dec ply) eval-fn)]
               [(- val) move]))))))))

(defn minimax-searcher [ply eval-fn]
  (fn [player board]
    (let [[_ move] (minimax player board ply eval-fn)]
      move)))

(def moves [[79 68] [68 57] [90 68] [68 71] [71 38] [38 16]])
(def idx (ref 0))

(defn human [player _]
  (let [move (nth moves @idx)]
    (dosync (alter idx inc) idx)
    move))
