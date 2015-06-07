(ns apl.core
  (:refer-clojure :exclude [= + / < >])
  (:require [clojure.core.matrix
              :refer [abs ceil exp log signum pow emap eseq ecount scalar? shape]]
            [clojure.core.matrix.operators :as m]))

;; utility
(defn- bool->int [x]
  (if x 1 0))

;; comparison functions
(def < (partial emap (comp bool->int clojure.core/<)))
(def ≤ (partial emap (comp bool->int <=)))
(def = (partial emap (comp bool->int clojure.core/=)))
(def ≥ (partial emap (comp bool->int >=)))
(def > (partial emap (comp bool->int clojure.core/>)))

;; logical functions
(defn ∧ [a b]
  (cond
    (and (clojure.core/= a 0) (clojure.core/= b 0)) 0
    (and (clojure.core/= a 0) (clojure.core/= b 1)) 0
    (and (clojure.core/= a 1) (clojure.core/= b 0)) 0
    (and (clojure.core/= a 1) (clojure.core/= b 1)) 1
    :else (throw (ArithmeticException. "Domain error"))))

(defn ∨ [a b]
  (cond
    (and (clojure.core/= a 0) (clojure.core/= b 0)) 0
    (and (clojure.core/= a 0) (clojure.core/= b 1)) 1
    (and (clojure.core/= a 1) (clojure.core/= b 0)) 1
    (and (clojure.core/= a 1) (clojure.core/= b 1)) 1
    :else (throw (ArithmeticException. "Domain error"))))

(defn ∼ [a]
  (letfn [(negate [x]
            (cond
              (clojure.core/= x 0) 1
              (clojure.core/= x 1) 0
              :else (throw (ArithmeticException. "Domain error"))))]
    (emap negate a)))

;; arithmetic functions
(defn ⋆
  ([a] (exp a))
  ([a b] (pow a b)))

(defn ⍟
  ([x] (log x))
  ([base x] (m// (log x) (log base))))

(def ∣ abs)

(defn ×
  ([x] (signum x))
  ([a b] (m/* a b)))

(defn ÷
  ([a] (m// 1 a))
  ([a b] (m// a b)))

(defn +
  ([a] a)
  ([a b] (m/+ a b)))

(defn ⌈
  ([a] (ceil a))
  ([a b] (Math/max a b)))

;; vector functions
(defn index [xs idx]
  (if (sequential? idx)
    (get-in xs (map dec idx))
    (nth xs (dec idx))))

(defn ⍴
  ([a] (if (scalar? a) [] (shape a)))
  ([[d & ds] xs]
    ; TODO: Case for empty dimensions vector
    (let [elements (if (clojure.core/= (ecount xs) 0)
                     (repeat 0)
                     (cycle (eseq xs)))
          contents (if (nil? ds)
                     elements
                     (map
                       (partial ⍴ ds)
                       (partition (apply * ds) elements)))]
      (vec (take d contents)))))

(defn ↑ [limit xs]
  ; TODO: Rank error if input is more than one dimension
  (case (signum limit)
    -1.0 (vec (take-last
                (- limit)
                (concat
                  (take (- (- limit) (count xs)) (repeat 0))
                  xs)))
    1.0  (vec (take limit (concat xs (repeat 0))))
    0.0  []))

(defn ↓ [limit xs]
  ; TODO: Rank error if input is more than one dimension
  (if (clojure.core/< limit 0)
    (vec (drop-last (- limit) xs))
    (vec (drop limit xs))))

;; generating vectors
(defn ⍳ [n]
  (if (>= n 0)
    (vec (range 1 (inc n)))
    (throw (ArithmeticException. "Domain error"))))

;; operators
(defn / [f xs]
  (reduce f xs))
