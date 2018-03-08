;; Unlike math.combinatorics, this is a more naive implementation that
;; doesn't filter out duplicate items.
;;
;; user> (permutations [1 2 3])
;; ((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1))
;; user> (permutations [1 1 2])
;; ((1 1 2) (1 2 1) (1 2 1) (1 1 2) (2 1 1) (2 1 1))
;;
;; user> (combo/permutations [1 1 2])
;; ([1 1 2] [1 2 1] [2 1 1])

(ns clojure-experiments.permutations)

(defn rotate
  "Rotates coll n steps"
  [coll n]
  (into (vec (drop n coll))
        (vec (take n coll))))

(defn rotations
  "Returns all rotations of passed coll"
  [coll]
  (reduce (fn [accum i]
            (conj accum (rotate coll i)))
          [] (range (count coll))))

(defn permutations
  [coll]
  (if (= (count coll) 1)
    (vector coll)
    (mapcat (fn [[head & tail]]
              (map (fn [perm]
                     (cons head perm))
                   (permutations tail)))
            (rotations coll))))

(permutations [1 2 3])
;; => ((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1))
