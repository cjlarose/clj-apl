(ns clj-apl.core
  (:require [clojure.core.matrix :refer [abs ceil exp log signum emap pow]]
            [clojure.core.matrix.operators :as m]))

;; comparison
(defn geq [a b]
  (if (>= a b) 1 0))

;; logical functions
(defn eq [a b]
  (if (= a b) 1 0))

(defn logical-conjunction [a b]
  (cond
    (and (= a 0) (= b 0)) 0
    (and (= a 0) (= b 1)) 0
    (and (= a 1) (= b 0)) 0
    (and (= a 1) (= b 1)) 1))

(defn logical-disjunction [a b]
  (cond
    (and (= a 0) (= b 0)) 0
    (and (= a 0) (= b 1)) 1
    (and (= a 1) (= b 0)) 1
    (and (= a 1) (= b 1)) 1))

(defn logical-not [a]
  (cond
    (= a 0) 1
    (= a 1) 0
    :else (throw (ArithmeticException. "Domain error"))))

;; arithmetic functions
(defn maximum [a b]
  (max a b))

(defn power
  ([a] (exp a))
  ([a b] (pow a b)))

(defn logarithm
  ([x] (log x))
  ([base x] (m// (log x) (log base))))

(def magnitude abs)

(defn signum-multiply
  ([x] (signum x))
  ([a b] (m/* a b)))

(defn divide
  ([a] (m// 1 a))
  ([a b] (m// a b)))

(defn add
  ([a] a)
  ([a b] (m/+ a b)))

(defn ceil-max
  ([a] (ceil a))
  ([a b] (Math/max a b)))

;; vector functions
(defn index [xs idx]
  (if (sequential? idx)
    (get-in xs (map dec idx))
    (nth xs (dec idx))))

(defn dimension
  ([a] count)
  ([new-dimension xs]
   (if (= (count new-dimension) 2)
     (let [[n m] new-dimension]
       (vec (take n (map vec (partition m (cycle xs)))))))))

(defn vec-take [limit xs]
  (case (signum limit)
    -1 (vec (take-last (- limit) xs))
    1  (vec (take limit xs))
    0  []))

(defn vec-drop [limit xs]
  (case (signum limit)
    -1 (vec (drop-last (- limit) xs))
    1  (vec (drop limit xs))
    0  xs))

;; generating vectors
(defn iota [n]
  (vec (range 1 (inc n))))

;; operators
(defn reduction [f xs]
  (reduce f xs))

;; decorators
(defn to-vector [x]
  (if (vector? x) x (repeat x)))

(defn apl-fn [sym]
  (condp = sym
    '≥ geq
    '= eq
    '∧ (partial emap logical-conjunction)
    '∨ (partial emap logical-disjunction)
    '∼ logical-not
    '⋆ power
    '⍟ logarithm
    '∣ magnitude
    '× signum-multiply
    '÷ divide
    '+ add
    '⍳ iota
    '/ reduction
    '⌈ ceil-max
    '⍴ dimension
    '↑ vec-take
    '↓ vec-drop))
