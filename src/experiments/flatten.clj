(ns clojure-experiments.flatten)

(def v [1 2 [3 4] [5 [6 [7] 8]]])

(defn my-flatten
  [v]
  (loop [[head & tail] v accum []]
    (if (nil? head)
      accum
      (if (coll? head)
        (recur tail (into accum (my-flatten head)))
        (recur tail (conj accum head))))))

(my-flatten v)
