(use '[clojure.contrib.str-utils :only (re-split)])
(use '[clojure.contrib.pprint :only (cl-format)])

(defmacro ns-docs [name]
  `(do
     ~@(map
        (comp (partial cons 'doc) list)
        (keys (ns-interns `~name)))))

(defn []
  (println
   (for [docs (re-split #"-------------------------"
                        (with-out-str (ns-docs shogi.core)))]
     (let [[_ name args & descriptions] (re-split #"\n" docs)]
       (cl-format true "** ~s~%~s~%~{~s~%~}~%" name args descriptions)))))
