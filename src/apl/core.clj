(ns apl.core
  (:refer-clojure :exclude [= + / < >])
  (:require [clojure.core.matrix
              :refer [abs ceil exp log signum pow emap eseq ecount scalar? shape array]]
            [clojure.core.matrix.operators :as m]))

;; utility
(defn- bool->int [x]
  (if x 1 0))

(defn- int->bool [x]
  (cond
    (clojure.core/= x 1) true
    (clojure.core/= x 0) false
    :else (throw (ArithmeticException. "Domain error"))))

;; comparison functions
(def < (partial emap (comp bool->int clojure.core/<)))
(def ≤ (partial emap (comp bool->int <=)))
(def = (partial emap (comp bool->int clojure.core/=)))
(def ≥ (partial emap (comp bool->int >=)))
(def > (partial emap (comp bool->int clojure.core/>)))

;; logical functions
(defn- with-bool-args [f]
  (fn [& args]
    (bool->int (apply f (map int->bool args)))))

(def ∨ (partial emap (with-bool-args (fn [x y] (or x y)))))
(def ∧ (partial emap (with-bool-args (fn [x y] (and x y)))))
(def ⍱ (partial emap (with-bool-args (fn [x y] (not (or x y))))))
(def ⍲ (partial emap (with-bool-args (fn [x y] (not (and x y))))))
(def ∼ (partial emap (with-bool-args not)))

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
  ([a b] (emap (fn [x y] (Math/max x y)) a b)))

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
      (array (take d contents)))))

(defn ↑ [limit xs]
  ; TODO: Rank error if input is more than one dimension
  (if (clojure.core/< limit 0)
    (array (take-last
           (- limit)
           (concat
             (take (- (- limit) (count xs)) (repeat 0))
             xs)))
    (array (take limit (concat xs (repeat 0))))))

(defn ↓ [limit xs]
  ; TODO: Rank error if input is more than one dimension
  (if (clojure.core/< limit 0)
    (array (drop-last (- limit) xs))
    (array (drop limit xs))))

;; generating vectors
(defn ⍳ [n]
  (if (>= n 0)
    (array (range 1 (inc n)))
    (throw (ArithmeticException. "Domain error"))))

;; operators
(defn / [f xs]
  (reduce f xs))
