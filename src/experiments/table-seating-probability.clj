;; Walking with Bec, this morning, she shared the observation that her
;; colleagues seam to self-sort, on sides of a conference table, by
;; gender.  What is the probability of such orderings?

(ns clojure-experiments.table-seating-probability
  (:require [clojure.spec.alpha :as s]
            [clojure.math.combinatorics :as combo]))

(defn calc-side-permutations
  [coll]
  {:pre [(s/valid? even? (count coll))]}
  (map (partial take (/ (count coll) 2))
       (combo/permutations coll)))

(defn calc-probabilities
  [id coll]
  (let [perms (calc-side-permutations coll)
        total (count perms)]
    (->>
     (frequencies (map (fn [p]
                         (get (frequencies p) id)) perms))
     (reduce-kv (fn [m n cnt]
                  (assoc m n (float (/ cnt total))))
                {}))))

(defn report-probabilities
  [m]
  (doseq [n (-> (keys m) sort reverse)]
    (println (format "%.3f probability of %d on one side" (get m n) n))))


;;
;; Examples
;;
(-> (calc-probabilities \f (concat (repeat 3 \f)
                                   (repeat 3 \m)
                                   (repeat 1 \o)
                                   (repeat 1 nil)))
    report-probabilities)

;; 0.071 probability of null on one side
;; 0.429 probability of 1 on one side
;; 0.429 probability of 2 on one side
;; 0.071 probability of 3 on one side


(-> (calc-probabilities \f (concat (repeat 4 \f)
                                   (repeat 3 \m)
                                   (repeat 1 \o)
                                   (repeat 2 nil)))
    report-probabilities)

;; 0.024 probability of null on one side
;; 0.238 probability of 1 on one side
;; 0.476 probability of 2 on one side
;; 0.238 probability of 3 on one side
;; 0.024 probability of 4 on one side
