(ns clojure-experiments.coin-flip)

(def flips [:h :t :t :h :t :h :h :h])

(defn count-flips
  [cnt head tail]
  (if (empty? tail)
    cnt
    (if (= head (first tail))
      (count-flips (+ cnt 1) (first tail) (rest tail))
      (count-flips cnt (first tail) (rest tail)))))

(count-flips 0 (first flips) (rest flips))


(defn count-flips-improved
  [flips]
  (loop [cnt 0, head (first flips), tail (rest flips)]
        (if (empty? tail)
          cnt
          (if (= head (first tail))
            (recur (+ cnt 1) (first tail) (rest tail))
            (recur cnt (first tail) (rest tail))))))

(count-flips-improved flips)

(defn count-flips-improved-2
  [flips]
  (letfn [(counter [cnt head tail]
            (if (empty? tail)              
              cnt
              (if (= head (first tail))
                (counter (+ cnt 1) (first tail) (rest tail))
                (counter cnt (first tail) (rest tail)))))]
    (counter 0 (first flips) (rest flips))))

(count-flips-improved-2 flips)

;; This version emits a msg when finding pairs.  I want to test
;; deconstructing the args without having to worry about passing cnt.

(defn count-flips-test-1
  [[head & tail]]
  ;;(println "head: " head " tail: " tail)
  (if (empty? tail)
    (println "we're done")
    (do
      (if (= head (first tail))
        (println "found a match"))
      (count-flips-test-1 tail))))

(count-flips-test-1 flips)


(defn count-flips-better
  [flips]
  (letfn [(counter [cnt tail]
            (let [head (first tail) tail (rest tail)]
              (if (empty? tail)              
                cnt
                (do
                  (if (= head (first tail))
                    (counter (+ cnt 1) tail)
                    (counter cnt tail))))))]
    (counter 0 flips)))

(count-flips-better flips)

