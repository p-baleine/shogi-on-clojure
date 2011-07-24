(ns shogi.webui_fb
  (:use [shogi.core] :reload)
  (:use [shogi.player] :reload)
  (:use clojure.contrib.math
        compojure.core
        ring.adapter.jetty ring.middleware.file
        ring.middleware.params ring.middleware.keyword-params
        sandbar.stateful-session
        ; ring.util.response
        clj-html.core clj-html.helpers)
  (:require [compojure.route :as route]
            [clj-json.core :as json])
  (:import java.io.StringReader java.io.BufferedReader))

(alias 'f 'shogi.core.first)
(alias 's 'shogi.core.second)

(defn fetch-image-filename [piece]
  (str "/images/"
     (condp contains? piece
       #{f/HUU s/HUU} "HUU" #{f/KYO s/KYO} "KYO"
       #{f/KEI s/KEI} "KEI" #{f/GIN s/GIN} "GIN"
       #{f/KIN s/KIN} "KIN" #{f/OUU s/OUU} "OUU"
       #{f/HIS s/HIS} "HIS" #{f/KAK s/KAK} "KAK")
     ".png"))

(defn make-captured-board [p]
  (let [squares (vec (captured-squares p))]
  (html
   [:table {:class (str "captured " (name p))}
     [:tr (for [square (subvec squares 0 5)]
            [:td.square {:id square}
             [:div.piece.first]])]
     [:tr (for [square (subvec squares 6)]
            [:td.square {:id square}
             [:div.piece.first]])]])))
       
(defn start-game []
  (session-put! :board (initial-board))
  (html
   ""
   [:html
    [:head
     [:title "Shogi on Clojure"]
     (include-css "/css/shogi_fb.css")
     (include-js "/scripts/lib/jquery-1.5.min.js")
     (include-js "/scripts/lib/jquery.ui.core.js")
     (include-js "/scripts/lib/jquery.ui.widget.js")
     (include-js "/scripts/lib/jquery.ui.mouse.js")
     (include-js "/scripts/lib/jquery.ui.draggable.js")
     (include-js "/scripts/lib/jquery.ui.droppable.js")
     (include-js "/scripts/lib/jquery.effects.core.js")
     (include-js "/scripts/lib/jquery.effects.highlight.js")
     (include-js "/scripts/shogi_fb.js")]
    [:body
     [:table 
      [:td [:tr.first (make-captured-board :first)]]
      [:td
       [:tr
       [:table.on-board
        (for [row (range 1 10)]
          [:tr
           (for [col (range 1 10)]
             (let [id (+ col (* 11 row))
                   piece (bref (initial-board) id)]
               [:td.square {:id id}
                [:div {:class (if (= :first (player piece))
                                "piece first"
                                "piece second")}
                 (when-not (= piece EMP)
                   [:img {:src (fetch-image-filename piece)}])]]))])]]]
      [:td [:tr.second (make-captured-board :second)]]
      ]]]))


;; (defn json-response [data & [status]]
;;   {:status (or status 200)
;;    :headers {"Content-Type" "application/json"}
;;    :body (json/generate-string data)})

(defn srv-random-strategy [player]
  (let [[from to]
        (random-strategy player (session-get :board))]
    (json/generate-string {"from" from "to" to})))

(defn srv-minimax-searcher [player]
  (let [[from to promotion]
        ((minimax-searcher 2 weighted-squares)
         player (session-get :board))]
    (json/generate-string {"from" from "to" to "promotion" promotion})))

(defn srv-make-move! [player from to promotion board]
  (let [[board captured]
        (make-move! from to promotion board)]
    (when captured
      (capture-piece! player captured board))
    (session-put! :board board)
    (print-board board)
    (if captured
      (json/generate-string {"captured" captured})
      (json/generate-string false))))

(defn srv-valid-move-p [player from to promotion board]
  (if (valid-move? player from to promotion board)
    (json/generate-string true)
    (json/generate-string false)))

(defn srv-highlight-moves [from board]
  (json/generate-string
   (vec (for [m (valid-moves from board)] {"to" m}))))

(defn srv-promotable-p [player from to board]
  (if (promotable? player from to board)
    (json/generate-string true)
    (json/generate-string false)))

(defroutes main-routes
  (POST "/start" [] (start-game))
  (POST "/srv-highlight-moves" {p :params}
       (srv-highlight-moves
        (Integer/parseInt (:from p)) (session-get :board)))
  (POST "/srv-valid-move-p" {p :params}
       (srv-valid-move-p
        (keyword (:player p)) (Integer/parseInt (:from p))
        (Integer/parseInt (:to p)) (Boolean/valueOf(:promotion p))
        (session-get :board)))
  (POST "/srv-promotable-p" {p :params}
       (srv-promotable-p 
        (keyword (:player p)) (Integer/parseInt (:from p))
        (Integer/parseInt (:to p)) (session-get :board)))
  (POST "/srv-make-move" {p :params}
        (srv-make-move!
         (keyword (:player p)) (Integer/parseInt (:from p))
         (Integer/parseInt (:to p)) (Boolean/valueOf(:promotion p))
         (session-get :board)))
  (POST "/srv-random-strategy" {p :params}
       (srv-random-strategy (keyword (:player p))))
  (POST "/srv-minimax-searcher" {p :params}
       (srv-minimax-searcher (keyword (:player p))))
  (route/not-found "<h1>Page not found</h1>"))

(def app
  (-> main-routes
      wrap-keyword-params
      wrap-params
      (wrap-file "/home/sabu/work/clojure/shogi")
      wrap-stateful-session))

;; (run-jetty (var app) {:port 8080})
