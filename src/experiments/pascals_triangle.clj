(ns clojure-experiments.pascals-triangle)

;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1

(defn node
  [x y]
  (cond
      (<= x 0) 1
      (>= x y) 1
      :else (+ (node (dec x) (dec y))
               (node x (dec y)))))

(defn mk-triangle
  [width]
  (loop [i 0]
    (when (< i width)
      (print (node i (dec width)))
      (recur (inc i)))))
