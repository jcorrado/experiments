(ns clojure-experiments.pascals-triangle
  (:require [clojure.string :as str]))

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

(def node-memo (memoize node))

(defn mk-triangle
  [width]
  (loop [i 0 accum []]
    (if (< i width)
      (recur (inc i) (conj accum (node-memo i (dec width))))
      accum)))

(defn print-triangle
  [width]  
  (print (str/join " " (mk-triangle width))))
