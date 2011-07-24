(ns shogi.test.core
  (:use [shogi.core] :reload)
  (:use [clojure.test]))

(deftest test-create-ns-with-vars
  (create-ns-with-bindings shogi.test.test-ns
    test-var1 'one test-var2 'two)
  (let [interns (ns-interns 'shogi.test.test-ns)]
    (is (and (= (var-get (interns 'test-var1))
                'one)
             (= (var-get (interns 'test-var2))
                'two)))))

(deftest test-valid-moves
  (is (= (valid-moves 34 (initial-board))
         '(45)))
  (is (= (valid-moves 12 (initial-board))
         '(23)))
  (is (= (valid-moves 13 (initial-board))
         '()))
  (is (= (valid-moves 14 (initial-board))
         '(26 25)))
  (is (= (valid-moves 15 (initial-board))
         '(27 26 25)))
  (is (= (valid-moves 24 (initial-board))
         '()))
  (is (= (valid-moves 30 (initial-board))
         '(31 29 28 27 26 25)))
  (is (= (let [b (initial-board)]
           (bset! b 0 (- (bref b 78)))
           (sort (valid-moves 0 b)))
         '(23 25 26 27 28 29 31 45 46
           47 48 49 50 51 52 53 56 57
           58 59 60 61 62 63 64 67 68
           69 70 71 72 73 74 75 89 91
           92 93 94 95 97)))
  (is (= (let [b (initial-board)]
           (bset! b 110 (- (bref b 42)))
           (sort (valid-moves 110 b)))
         '(23 25 26 27 28 29 31 45 46
           47 48 49 50 51 52 53 56 57
           58 59 60 61 62 63 64 67 68
           69 70 71 72 73 74 75 89 91
           92 93 94 95 97))))

(deftest test-get-moves
  (let [board (initial-board)]
    (get-move (fn [_ _] [36 47 false]) :second board)
    (is (= [0  0  0  0  0  0  0  0  0  0  0
            0 -2 -3 -4 -5 -6 -5 -4 -3 -2  0
            0  0 -8  0  0  0  0  0 -7  0  0
            0 -1 -1  0 -1 -1 -1 -1 -1 -1  0
            0  0  0 -1  0  0  0  0  0  0  0
            0  0  0  0  0  0  0  0  0  0  0
            0  0  0  0  0  0  0  0  0  0  0
            0  1  1  1  1  1  1  1  1  1  0
            0  0  7  0  0  0  0  0  8  0  0
            0  2  3  4  5  6  5  4  3  2  0
            0  0  0  0  0  0  0  0  0  0  0]
             @board))
    (get-move (fn [_ _] [78 67 false]) :first board)
    (is (= [0  0  0  0  0  0  0  0  0  0  0
            0 -2 -3 -4 -5 -6 -5 -4 -3 -2  0
            0  0 -8  0  0  0  0  0 -7  0  0
            0 -1 -1  0 -1 -1 -1 -1 -1 -1  0
            0  0  0 -1  0  0  0  0  0  0  0
            0  0  0  0  0  0  0  0  0  0  0
            0  1  0  0  0  0  0  0  0  0  0
            0  0  1  1  1  1  1  1  1  1  0
            0  0  7  0  0  0  0  0  8  0  0
            0  2  3  4  5  6  5  4  3  2  0
            0  0  0  0  0  0  0  0  0  0  0]
             @board))
    (get-move (fn [_ _] [24 84 true]) :second board)
    (is (= [0  0  0  0  0  0  0  0  0  0  0
            0 -2 -3 -4 -5 -6 -5 -4 -3 -2  0
            0  0  0  0  0  0  0  0 -7  0  0
            0 -1 -1  0 -1 -1 -1 -1 -1 -1  0
            0  0  0 -1  0  0  0  0  0  0  0
            0  0  0  0  0  0  0  0  0  0  0
            0  1  0  0  0  0  0  0  0  0  0
            0  0  1  1  1  1  1 -80  1  1  0
            0  0  7  0  0  0  0  0  8  0  0
            0  2  3  4  5  6  5  4  3  2  0
            -1  0  0  0  0  0  0  0  0  0  0]
             @board))
   (get-move (fn [_ _] [84 83 false]) :second board)
    (is (= [0  0  0  0  0  0  0  0  0  0  0
            0 -2 -3 -4 -5 -6 -5 -4 -3 -2  0
            0  0  0  0  0  0  0  0 -7  0  0
            0 -1 -1  0 -1 -1 -1 -1 -1 -1  0
            0  0  0 -1  0  0  0  0  0  0  0
            0  0  0  0  0  0  0  0  0  0  0
            0  1  0  0  0  0  0  0  0  0  0
            0  0  1  1  1  1 -80 0  1  1  0
            0  0  7  0  0  0  0  0  8  0  0
            0  2  3  4  5  6  5  4  3  2  0
            -1 -1  0  0  0  0  0  0  0  0  0]
             @board))
    ;; (get-move (fn [_ _] [96 84 false]) :first board)
    ;; (is (= [8  0  0  0  0  0  0  0  0  0  0
    ;;         0 -2 -3 -4 -5 -6 -5 -4 -3 -2  0
    ;;         0  0  0  0  0  0  0  0 -7  0  0
    ;;         0 -1 -1  0 -1 -1 -1 -1 -1 -1  0
    ;;         0  0  0 -1  0  0  0  0  0  0  0
    ;;         0  0  0  0  0  0  0  0  0  0  0
    ;;         0  1  0  0  0  0  0  0  0  0  0
    ;;         0  0  1  1  1  1  1  8  1  1  0
    ;;         0  0  7  0  0  0  0  0  0  0  0
    ;;         0  2  3  4  5  6  5  4  3  2  0
    ;;         -1  0  0  0  0  0  0  0  0  0  0]
    ;;          @board))
    ))
