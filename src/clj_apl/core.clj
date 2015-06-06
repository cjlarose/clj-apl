(ns clj-apl.core
  (:refer-clojure :exclude [= + /])
  (:require [clojure.core.matrix :refer [abs ceil exp log signum emap pow]]
            [clojure.core.matrix.operators :as m]))

;; comparison
(defn ≥ [a b]
  (if (>= a b) 1 0))

;; logical functions
(defn = [a b]
  (if (clojure.core/= a b) 1 0))

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
  (cond
    (clojure.core/= a 0) 1
    (clojure.core/= a 1) 0
    :else (throw (ArithmeticException. "Domain error"))))

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
  ([a] count)
  ([new-dimension xs]
   (if (clojure.core/= (count new-dimension) 2)
     (let [[n m] new-dimension]
       (vec (take n (map vec (partition m (cycle xs)))))))))

(defn ↑ [limit xs]
  (case (signum limit)
    -1.0 (vec (take-last (- limit) xs))
    1.0  (vec (take limit xs))
    0.0  []))

(defn ↓ [limit xs]
  (case (signum limit)
    -1.0 (vec (drop-last (- limit) xs))
    1.0  (vec (drop limit xs))
    0.0  xs))

;; generating vectors
(defn ⍳ [n]
  (if (>= n 0)
    (vec (range 1 (inc n)))
    (throw (ArithmeticException. "Domain error"))))

;; operators
(defn / [f xs]
  (reduce f xs))
