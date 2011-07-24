(ns shogi.core
  (:use clojure.contrib.math
        clojure.contrib.logging))

;; Game board is represented by array of integer,
;; where 0 indicates the empty square, 1 indicates the Huu of first player,
;; -1 indicates the Huu of second player... and so on.

;; The position of game board is representedas index of this array.
;; The whole indices of this board is as follows.

;;   0   1   2   3   4   5   6   7   8   9  10
;;  11  12  13  14  15  16  17  18  19  20  21
;;  22  23  24  25  26  27  28  29  30  31  32
;;  33  34  35  36  37  38  39  40  41  42  43
;;  44  45  46  47  48  49  50  51  52  53  54
;;  55  56  57  58  59  60  61  62  63  64  65
;;  66  67  68  69  70  71  72  73  74  75  76
;;  77  78  79  80  81  82  83  84  85  86  87
;;  88  89  90  91  92  93  94  95  96  97  98
;;  99 100 101 102 103 104 105 106 107 108 109
;; 110 111 112 113 114 115 116 117 118 119 120

;; Where first row (1-11), last row (111-121),
;; first column (1-111) and last column (11-121) represent the out of board.

;; Captured pieces are included in this array,
;; first row (1-11) is second player's captured pieces and
;; last row (11-121) is first player's captured pieces.

(def EMP 0)

(defmacro create-ns-with-bindings [ns-name & bindings]
  "Create NS-NAME namespace which has definitions of VARS."
  `(do (create-ns '~ns-name)
       ~@(for [[vars# vals#] (partition 2 bindings)]
           `(intern '~ns-name '~vars# ~vals#))))

(create-ns-with-bindings shogi.core.first
  HUU 1 KYO 2 KEI 3 GIN 4 KIN 5 OUU 6 HIS 7 KAK 8
  TKIN 10 NKYO 20 NKEI 30 NGIN 40 RYUMA 80 RYUOU 70)

(create-ns-with-bindings shogi.core.second
  HUU -1 KYO -2 KEI -3 GIN -4 KIN -5 OUU -6 HIS -7 KAK -8
  TKIN -10 NKYO -20 NKEI -30 NGIN -40 RYUMA -80 RYUOU -70)

(defn promoted? [piece] (>= (abs piece) 10))
(defn promote [piece] (* 10 piece))
(defn un-promote [piece] (/ piece 10))

(alias 'f 'shogi.core.first)
(alias 's 'shogi.core.second)

(defn initial-board []
  "Return the initial board of Shogi game."
  (ref
   [ EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP  EMP
     EMP s/KYO s/KEI s/GIN s/KIN s/OUU s/KIN s/GIN s/KEI s/KYO  EMP
     EMP   EMP s/KAK   EMP   EMP   EMP   EMP   EMP s/HIS   EMP  EMP
     EMP s/HUU s/HUU s/HUU s/HUU s/HUU s/HUU s/HUU s/HUU s/HUU  EMP
     EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP  EMP
     EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP  EMP
     EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP  EMP
     EMP f/HUU f/HUU f/HUU f/HUU f/HUU f/HUU f/HUU f/HUU f/HUU  EMP
     EMP   EMP f/HIS   EMP   EMP   EMP   EMP   EMP f/KAK   EMP  EMP
     EMP f/KYO f/KEI f/GIN f/KIN f/OUU f/KIN f/GIN f/KEI f/KYO  EMP
     EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP   EMP  EMP]))

(defmacro forever [x] `(iterate #(+ ~x %) ~x))

(def
  ^{:doc "Map that consist of piece and direction of piece.
Each direction of a piece is represented as a list which contain
a integer or a list of integer."}
  directions
  {
   s/HUU '((+11))
   s/KYO `(~(forever +11))
   s/KEI '((+23 +21))
   s/GIN '((+12) (+11) (+10) (-12) (-10))
   s/KIN '((+12) (+11) (+10) (-1) (-11) (+1))
   s/OUU '((-1) (-12) (-11) (-10) (+1) (+12) (+11) (+10))
   s/HIS `(~(forever +1) ~(forever +11) ~(forever -1) ~(forever -11))
   s/KAK `(~(forever -12) ~(forever -10) ~(forever +10) ~(forever +12))
   s/TKIN '((+12) (+11) (+10) (-1) (-11) (+1))
   s/NKYO '((+12) (+11) (+10) (-1) (-11) (+1))
   s/NKEI '((+12) (+11) (+10) (-1) (-11) (+1))
   s/NGIN '((+12) (+11) (+10) (-1) (-11) (+1))
   s/RYUMA `((+11) (-1) (-11) (+1)
             ~(forever -12) ~(forever -10) ~(forever +10) ~(forever +12))
   s/RYUOU `((+12) (+10) (-12) (-10)
             ~(forever +1) ~(forever +11) ~(forever -1) ~(forever -11))

   f/HUU '((-11))
   f/KYO `(~(forever -11))
   f/KEI '((-21 -23))
   f/GIN '((-12) (-11) (-10) (+12) (+10))
   f/KIN '((-12) (-11) (-10) (-1) (+1) (+11))
   f/OUU '((+1) (+12) (+11) (+10) (-1) (-12) (-11) (-10))
   f/HIS `(~(forever -1) ~(forever -11) ~(forever +1) ~(forever +11))
   f/KAK `(~(forever -12) ~(forever -10) ~(forever +10) ~(forever +12))
   f/TKIN '((-12) (-11) (-10) (-1) (+1) (+11))
   f/NKYO '((-12) (-11) (-10) (-1) (+1) (+11))
   f/NKEI '((-12) (-11) (-10) (-1) (+1) (+11))
   f/NGIN '((-12) (-11) (-10) (-1) (+1) (+11))
   f/RYUMA `((-11) (-1) (+1) (+11)
             ~(forever -12) ~(forever -10) ~(forever +10) ~(forever +12))
   f/RYUOU `((-12) (-10) (+12) (+10)
             ~(forever -1) ~(forever -11) ~(forever +1) ~(forever +11))
   })

(defn bref
  "Refer SQUARE of BOARD."
  [board #^Integer square] (nth @board square))

(defn bset!
  "Set SQUARE of BOARD with PIECE."
  [board square piece]
  (dosync (alter board assoc square piece)) board)

(defn name-of [piece]
  "Return the CUI representation of PIECE."
  (if-not (promoted? piece)
    (.charAt "□歩香桂銀金王飛角" (abs piece))
    (.charAt "　と杏圭全　　竜馬" (/ (abs piece) 10))))

(defn print-board
  "Print a board, along with captured pieces of first and second player."
  [board]
  (printf "\n      1  2  3  4  5  6  7  8  9")
  (doseq [row (range 1 10)]
    (printf "\n  %d " (* 11 row))
    (doseq [col (range 1 10)]
      (printf " %s" (name-of (bref board (+ col (* 11 row)))))))
  (printf "\n\n  First's capt-pieces: %s"
          (apply str (map #(name-of (bref board %)) (range 0 10))))
  (printf "\n  Second's capt-pieces:%s"
          (apply str (map #(name-of (bref board %)) (range 110 120))))
  (newline))

(defn player [piece]
  "Return keyword which indicates owner of PIECE."
  (cond (pos? piece) :first
        (neg? piece) :second
        :else false))

(defn opponent [p]
  (if (= p :first) :second :first))

(defn emp?
  "Is SQUARE on BOARD empty?"
  [board square] (= 0 (bref board square)))

(defn on-board-squares
  "Return set of squares which are on the game board."
  [] (set (filter #(<= 1 (mod % 11) 9) (range 12 109))))

(defn captured-squares
  "Return set of squares which are assigned to PLAYER."
  [player]
  (case player
        :first (set (range 0 11))
        :second (set (range 110 121))))

(defn camp-squares [player]
  (case player
        :second (set (filter #(<= 1 (mod % 11) 9) (range 12 43)))
        :first (set (filter #(<= 1 (mod % 11) 9) (range 78 109)))))

(defn square-taxonomy
  "Return a category of SQUARE based on BOARD."
  [square board]
  (cond ((on-board-squares) square) :on-board
        (or ((captured-squares :first) square)
            ((captured-squares :second) square))
        :captured))

(declare valid-move? valid-moves all-moves promotable?)

(defn valid-move?
  "Can P move piece at FROM to TO on BOARD?"
  [p from to promotion board]
  (and (= p (player (bref board from)))
       ((set (valid-moves from board)) to)
       (if promotion
         (promotable? p from to board)
         true)))

(defmulti valid-moves
  "Return a list of moves which piece at FROM can be moved to on BOARD."
  {:arglists '([from board])}
  square-taxonomy)

(defmethod valid-moves :on-board [from board]
  (apply
   concat
   (for [moves (all-moves from board)]
     (let [[emps rest]
           (split-with (partial emp? board) moves)]
       (if (or (empty? rest)
               (= (player (bref board from))
                  (player (bref board (first rest)))))
         emps
         (into emps (list (first rest))))))))

(defn- all-moves [from board]
  (map (fn [move]
         (take-while (on-board-squares) (map #(+ from %) move)))
       (directions (bref board from))))

(defmethod valid-moves :captured [from board]
  (if-not (emp? board from)
    (filter (partial emp? board) (on-board-squares))
    '()))

(defmethod valid-moves :default [from board] '())

(defn promotable? [player from to board]
  (let [piece (bref board from)
        opp-camp (camp-squares (opponent player))]
    (and (not (promoted? piece))
         (if (= player :first)
           ((complement #{f/OUU f/KIN}) piece)
           ((complement #{s/OUU s/KIN}) piece))
         (or (and ((complement opp-camp) from)
                  (opp-camp to))
             (and (opp-camp from)
                  (opp-camp to))
             (and (opp-camp from)
                  ((complement opp-camp) to))))))

(defn make-move!
  "Move piece at FROM to TO on BOARD."
  [from to promotion board]
  (let [old-piece (bref board to)
        new-piece (if promotion
                    (promote (bref board from))
                    (bref board from))]
    (bset! board to new-piece)
    (bset! board from EMP)
    [board (if (= EMP old-piece) false old-piece)]))

(defn capture-piece!
  "Capture PIECE as player's captured-pieces.
Note: captured-pieces of player are included in BOARD."
  [player piece board]
  (bset! board
         (first
          (filter (partial emp? board)
                  (captured-squares player)))
         (- (if (promoted? piece)
              (un-promote piece) piece))))

(defn get-move [strategy player board]
  ;; (print-board board)
  (log :info (print-board board))
  (let [[from to promotion] (strategy player (ref (vec @board)))]
    (if (valid-move? player from to promotion board)
      (let [[new-board old-piece] (make-move! from to promotion board)]
        (when old-piece
          (capture-piece! player old-piece new-board))
        new-board))))

(defn next-to-play [board previous-player]
  (let [ouu (if (= :first previous-player) f/OUU s/OUU)]
    (when-not
        (some #(= ouu (bref board %))
              (captured-squares previous-player))
      (if (= previous-player :first) :second :first))))

(defn shogi [fst-strategy scd-strategy]
  (let [board (initial-board)]
    (loop [player :first
           strategy fst-strategy]
      (when-not (nil? player)
        (get-move strategy player board)
        (recur (next-to-play board player)
               (if (= player :first) scd-strategy fst-strategy))))
    (println "Game is end.")))
