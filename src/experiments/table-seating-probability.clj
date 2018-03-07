;; Walking with Bec, this morning, she shared the observation that her
;; colleagues seam to self-sort, on sides of a conference table, by
;; gender.  What is the probability of such orderings?

(ns clojure-experiments.table-seating-probability
  (:require [clojure.spec.alpha :as s]
            [clojure.math.combinatorics :as combo]))

(defn calc-side-perms
  [coll]
  {:pre [(s/valid? even? (count coll))]}
  (map (partial take (/ (count coll) 2))
       (combo/permutations coll)))

(defn report-probabilities
  [perms]
  (let [tot (count perms)]
    (doseq [n (range (-> (first perms) count inc))]
      (let [cnt (count (filter #(= n (get (frequencies %) \w)) perms))]
        (println (format "%.2f probability of %d women"
                         (float (/ cnt tot))
                         n))))))

(-> (calc-side-perms (concat (repeat 4 \w)
                             (repeat 4 \m)
                             (repeat 2 nil)))
    report-probabilities)

;; 0.00 probability of 0 women
;; 0.24 probability of 1 women
;; 0.48 probability of 2 women
;; 0.24 probability of 3 women
;; 0.02 probability of 4 women
;; 0.00 probability of 5 women
