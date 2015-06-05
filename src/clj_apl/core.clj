(ns clj-apl.core
  (:require [clojure.math.numeric-tower :as math]))

; (def ceil math/ceil)
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

(defn divide
  ([a] (/ 1 a))
  ([a b] (/ a b)))

(defn add
  ([a] a)
  ([a b] (+ a b)))

;; generating vectors
(defn iota [n]
  (vec (range 1 (inc n))))

;; decorators
(defn to-vector [x]
  (if (vector? x) x (repeat x)))

(defn element-wise [f]
  (fn
    ([a] f)
    ([a b] (if (some vector? [a b])
             (vec (map f (to-vector a) (to-vector b)))
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
    '÷ divide
    '+ (element-wise add)
    '⍳ iota))
