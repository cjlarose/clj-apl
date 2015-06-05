(ns clj-apl.core
  (:require [clojure.math.numeric-tower :as math]))

;
; (def floor math/floor)
;
;
; (def negate -)
;
; (def id identity)
;
; (defn sign [x]
;   (cond (> x 0) 1
;         (< x 0) -1
;         :else 0))
;
; (def reciprocal (partial / 1))
;
; (def pi-times (partial * Math/PI))

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
    (= a 1) 0))

;; arithmetic functions
(defn maximum [a b]
  (max a b))

(defn power
  ([a] (math/expt Math/E a))
  ([a b] (math/expt a b)))

(defn logarithm
  ([x] (Math/log x))
  ([base x] (/ (Math/log x) (Math/log base))))

(def magnitude math/abs)

(defn- signum [x]
  (cond (> x 0) 1
        (< x 0) -1
        :else 0))

(defn signum-multiply
  ([x] (signum x))
  ([a b] (* a b)))

(defn divide
  ([a] (/ 1 a))
  ([a b] (/ a b)))

(defn add
  ([a] a)
  ([a b] (+ a b)))

(defn ceil-max
  ([a] (math/ceil a))
  ([a b] (Math/max a b)))

;; vector functions
(defn index [xs idx]
  (nth xs (dec idx)))

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

(defn element-wise [f]
  (fn
    ([a] f)
    ([a b] (if (some vector? [a b])
             (vec (map (element-wise f) (to-vector a) (to-vector b)))
             (f a b)))))

(defn apl-fn [sym]
  (condp = sym
    '≥ geq
    '= eq
    '∧ (element-wise logical-conjunction)
    '∨ (element-wise logical-disjunction)
    '∼ logical-not
    '⋆ power
    '⍟ logarithm
    '∣ magnitude
    '× (element-wise signum-multiply)
    '÷ divide
    '+ (element-wise add)
    '⍳ iota
    '/ reduction
    '⌈ ceil-max
    '⍴ dimension
    '↑ vec-take
    '↓ vec-drop))
