(ns apl.core-test
  (:require [clojure.test :refer :all]
            [apl.core :as apl]))

(defn float=
  "http://gettingclojure.wikidot.com/cookbook:numbers"
  ([x y] (float= x y 0.00001))
  ([x y epsilon]
     (let [scale (if (or (zero? x) (zero? y)) 1 (Math/abs x))]
       (<= (Math/abs (- x y)) (* scale epsilon)))) )

;; Monadic functions
(deftest reciprocal
  (is (= 0.5 (apl/÷ 2.0)))
  (is (= 0.25 (apl/÷ 4.0)))
  (is (= 0.125 (apl/÷ 8.0)))
  (is (= [0.5 0.25 0.125] (apl/÷ [2.0 4.0 8.0])))
  (is (= [[0.5 0.25] [0.125 0.5]] (apl/÷ [[2.0 4.0] [8.0 2.0]])))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/÷ 0))))

(deftest negation
  (is (= 1 (apl/∼ 0)))
  (is (= 0 (apl/∼ 1)))
  (is (= [1 0 1 0 0] (apl/∼ [0 1 0 1 1])))
  (is (= [[1 0] [1 1] [0 1]] (apl/∼ [[0 1] [0 0] [1 0]])))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∼ 5)))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∼ -1))))

(deftest powers-of-e
  (is (= 1.0 (apl/⋆ 0.0)))
  (is (= Math/E (apl/⋆ 1.0)))
  (is (float= 0.36787944117144233 (apl/⋆ -1.0)))
  (is (float= 735.0951892419727 (apl/⋆ 6.6)))
  (is (= [1.0 Math/E Math/E 1.0] (apl/⋆ [0.0 1.0 1.0 0.0]))))

(deftest natural-log
  (is (= 0.0 (apl/⍟ 1.0)))
  (is (float= 2.0149030205422647 (apl/⍟ 7.5)))
  (is (= 1.0 (apl/⍟ Math/E)))
  (is (= [0.0 1.0 1.0 0.0] (apl/⍟ [1.0 Math/E Math/E 1.0])))
  (is (= 0.0 (apl/⍟ -1.0)))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍟ 0.0))))

(deftest signum
  (is (= 1.0 (apl/× 1.0)))
  (is (= 1.0 (apl/× 234.4)))
  (is (= -1.0 (apl/× -564.2)))
  (is (= -1.0 (apl/× -1.2)))
  (is (= 0.0 (apl/× 0.0)))
  (is (= [1.0 0.0 -1.0] (apl/× [5.0 0.0 -45.2]))))

(deftest identity-fn
  (is (= 1.0 (apl/+ 1.0)))
  (is (= 234.4 (apl/+ 234.4)))
  (is (= -564.2 (apl/+ -564.2)))
  (is (= -1.2 (apl/+ -1.2)))
  (is (= 0.0 (apl/+ 0.0)))
  (is (= [1.0 234.4 -564.2 -1.2 0.0] (apl/+ [1.0 234.4 -564.2 -1.2 0.0]))))

(deftest magnitude
  (is (= 5.0 (apl/∣ 5.0)))
  (is (= 45.3 (apl/∣ -45.3)))
  (is (= 0.0 (apl/∣ 0.0)))
  (is (= [5.0 45.3 0.0] (apl/∣ [5.0 45.3 0.0]))))

(deftest ceiling
  (is (= 5.0 (apl/⌈ 4.5)))
  (is (= 5.0 (apl/⌈ 5.0)))
  (is (= -5.0 (apl/⌈ -5.5)))
  (is (= 0.0 (apl/⌈ 0.0)))
  (is (= [5.0 2.0 -7.0 0.0] (apl/⌈ [4.5 1.2 -7.23 0.0]))))

(deftest first-n-natural-numbers
  (is (= [1 2 3 4 5] (apl/⍳ 5)))
  (is (= [1 2] (apl/⍳ 2)))
  (is (= [1] (apl/⍳ 1)))
  (is (= [] (apl/⍳ 0)))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍳ -1))))

(deftest shape
  (is (= [] (apl/⍴ 5)))
  (is (= [1] (apl/⍴ [1])))
  (is (= [4] (apl/⍴ [1 2 3 4])))
  (is (= [3 4] (apl/⍴ [[1 2 3 4] [4 5 6 7] [7 8 9 10]])))
  (is (= [4 3 2] (apl/⍴ [[[1 1] [2 2] [3 3]]
                         [[1 1] [2 2] [3 3]]
                         [[1 1] [2 2] [3 3]]
                         [[1 1] [2 2] [3 3]]]))))

;; Dyadic functions
(deftest strictly-less-than
  (is (= 1 (apl/< -2.0 -1.9)))
  (is (= 0 (apl/< -2.0 -2.0)))
  (is (= 0 (apl/< -2.0 -2.1)))
  (is (= 0 (apl/< 2.0 1.9)))
  (is (= 0 (apl/< 2.0 2.0)))
  (is (= 1 (apl/< 2.0 2.1)))
  (is (= [1 0 0] (apl/< -2.0 [-1.9 -2.0 -2.1])))
  (is (= [0 0 1] (apl/< 2.0 [1.9 2.0 2.1])))
  (is (= [0 0 1] (apl/< [-2.0 0.0 2.0] [-2.1 0.0 2.1]))))

(deftest less-than-or-equal-to
  (is (= 1 (apl/≤ -2.0 -1.9)))
  (is (= 1 (apl/≤ -2.0 -2.0)))
  (is (= 0 (apl/≤ -2.0 -2.1)))
  (is (= 0 (apl/≤ 2.0 1.9)))
  (is (= 1 (apl/≤ 2.0 2.0)))
  (is (= 1 (apl/≤ 2.0 2.1)))
  (is (= [1 1 0] (apl/≤ -2.0 [-1.9 -2.0 -2.1])))
  (is (= [0 1 1] (apl/≤ 2.0 [1.9 2.0 2.1])))
  (is (= [0 1 1] (apl/≤ [-2.0 0.0 2.0] [-2.1 0.0 2.1]))))

(deftest equality
  (is (= 0 (apl/= -2.0 -1.9)))
  (is (= 1 (apl/= -2.0 -2.0)))
  (is (= 0 (apl/= -2.0 -2.1)))
  (is (= 0 (apl/= 2.0 1.9)))
  (is (= 1 (apl/= 2.0 2.0)))
  (is (= 0 (apl/= 2.0 2.1)))
  (is (= [0 1 0] (apl/= -2.0 [-1.9 -2.0 -2.1])))
  (is (= [0 1 0] (apl/= 2.0 [1.9 2.0 2.1])))
  (is (= [0 1 0] (apl/= [-2.0 0.0 2.0] [-2.1 0.0 2.1]))))

(deftest greater-than-or-equal-to
  (is (= 0 (apl/≥ -2.0 -1.9)))
  (is (= 1 (apl/≥ -2.0 -2.0)))
  (is (= 1 (apl/≥ -2.0 -2.1)))
  (is (= 1 (apl/≥ 2.0 1.9)))
  (is (= 1 (apl/≥ 2.0 2.0)))
  (is (= 0 (apl/≥ 2.0 2.1)))
  (is (= [0 1 1] (apl/≥ -2.0 [-1.9 -2.0 -2.1])))
  (is (= [1 1 0] (apl/≥ 2.0 [1.9 2.0 2.1])))
  (is (= [1 1 0] (apl/≥ [-2.0 0.0 2.0] [-2.1 0.0 2.1]))))

(deftest strictly-greater-than
  (is (= 0 (apl/> -2.0 -1.9)))
  (is (= 0 (apl/> -2.0 -2.0)))
  (is (= 1 (apl/> -2.0 -2.1)))
  (is (= 1 (apl/> 2.0 1.9)))
  (is (= 0 (apl/> 2.0 2.0)))
  (is (= 0 (apl/> 2.0 2.1)))
  (is (= [0 0 1] (apl/> -2.0 [-1.9 -2.0 -2.1])))
  (is (= [1 0 0] (apl/> 2.0 [1.9 2.0 2.1])))
  (is (= [1 0 0] (apl/> [-2.0 0.0 2.0] [-2.1 0.0 2.1]))))

(deftest logical-or
  (is (= 0 (apl/∨ 0 0)))
  (is (= 1 (apl/∨ 0 1)))
  (is (= 1 (apl/∨ 1 0)))
  (is (= 1 (apl/∨ 1 1)))
  (is (= [1 1] (apl/∨ 1 [0 1])))
  (is (= [0 1] (apl/∨ 0 [0 1])))
  (is (= [0 1 1 1] (apl/∨ [0 0 1 1] [0 1 0 1])))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∨ 1 -1)))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∨ -5 0))))

(deftest logical-and
  (is (= 0 (apl/∧ 0 0)))
  (is (= 0 (apl/∧ 0 1)))
  (is (= 0 (apl/∧ 1 0)))
  (is (= 1 (apl/∧ 1 1)))
  (is (= [0 1] (apl/∧ 1 [0 1])))
  (is (= [0 0] (apl/∧ 0 [0 1])))
  (is (= [0 0 0 1] (apl/∧ [0 0 1 1] [0 1 0 1])))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∧ 1 -1)))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∧ -1 0))))

(deftest logical-nor
  (is (= 1 (apl/⍱ 0 0)))
  (is (= 0 (apl/⍱ 0 1)))
  (is (= 0 (apl/⍱ 1 0)))
  (is (= 0 (apl/⍱ 1 1)))
  (is (= [0 0] (apl/⍱ 1 [0 1])))
  (is (= [1 0] (apl/⍱ 0 [0 1])))
  (is (= [1 0 0 0] (apl/⍱ [0 0 1 1] [0 1 0 1])))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍱ 1 -1)))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍱ -5 0))))

(deftest logical-nand
  (is (= 1 (apl/⍲ 0 0)))
  (is (= 1 (apl/⍲ 0 1)))
  (is (= 1 (apl/⍲ 1 0)))
  (is (= 0 (apl/⍲ 1 1)))
  (is (= [1 0] (apl/⍲ 1 [0 1])))
  (is (= [1 1] (apl/⍲ 0 [0 1])))
  (is (= [1 1 1 0] (apl/⍲ [0 0 1 1] [0 1 0 1])))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍲ 1 -1)))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍲ -1 0))))

(deftest exponentiation
  (is (float= 4.0 (apl/⋆ 2.0 2.0)))
  (is (float= 8.0 (apl/⋆ 2.0 3.0)))
  (is (float= 9.0 (apl/⋆ 3.0 2.0)))
  (is (float= 1.0 (apl/⋆ 3.0 0.0)))
  (is (float= 3.0 (apl/⋆ 3.0 1.0)))
  (is (float= 0.125 (apl/⋆ 2.0 -3.0)))
  (is (every? true? (map float= [4.0 8.0 0.125] (apl/⋆ 2.0 [2.0 3.0 -3.0]))))
  (is (every? true? (map float= [4.0 8.0 9.0 3.0 0.125] (apl/⋆ [2.0 2.0 3.0 3.0 2.0] [2.0 3.0 2.0 1.0 -3.0])))))

(deftest logarithms
  (is (float= 1.0 (apl/⍟ 2.0 2.0)))
  (is (float= 2.0 (apl/⍟ 2.0 4.0)))
  (is (float= 3.0 (apl/⍟ 2.0 8.0)))
  (is (float= 4.0 (apl/⍟ 2.0 16.0)))
  (is (float= 3.0 (apl/⍟ 3.0 27.0)))
  (is (every? true? (map float= [1.0 2.0 3.0 4.0] (apl/⍟ 2.0 [2.0 4.0 8.0 16.0]))))
  (is (every? true? (map float= [1.0 2.0 3.0] (apl/⍟ [2.0 3.0 4.0] [2.0 9.0 64.0]))))
  (testing "log base zero"
    (is (float= 0.0 (apl/⍟ 0.0 1.0)))
    (is (float= 0.0 (apl/⍟ 0.0 25.2)))
    (is (float= 0.0 (apl/⍟ 0.0 0.0))))
  (testing "log base one"
    (is (float= 1.0 (apl/⍟ 1.0 1.0)))
    (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍟ 1.0 25.2)))
    (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/⍟ 1.0 0.0))))
  (testing "log base e"
    (is (float= 0.0 (apl/⍟ Math/E -1.0)))
    (is (float= 0.0 (apl/⍟ Math/E 0.0)))
    (is (float= 0.0 (apl/⍟ Math/E 1.0)))
    (is (float= 1.0 (apl/⍟ Math/E Math/E)))
    (is (float= 3.226843995 (apl/⍟ Math/E 25.2))))
  (testing "returns 0.0 for undefined results"
    (is (float= 0.0 (apl/⍟ 2.0 -1.0))))
  (testing "returns negative infinity"
    (is (= Double/NEGATIVE_INFINITY (apl/⍟ 2.0 0.0)))
    (is (= Double/NEGATIVE_INFINITY (apl/⍟ 8.0 0.0)))))

(deftest addition
  (is (float= 4.0 (apl/+ 2.0 2.0)))
  (is (float= 5.0 (apl/+ 2.0 3.0)))
  (is (float= 10.5 (apl/+ 11.0 -0.5)))
  (is (float= 11.0 (apl/+ 11.0 0.0)))
  (is (every? true? (map float= [4.0 5.0 10.5 11.0] (apl/+ 4.5 [-0.5 0.5 6.0 6.5]))))
  (is (every? true? (map float= [7.0 9.0 11.0 13.0 15.0] (apl/+ [1.0 2.0 3.0 4.0 5.0] [6.0 7.0 8.0 9.0 10.0])))))

(deftest multiplication
  (is (float= 4.0 (apl/× 2.0 2.0)))
  (is (float= 6.0 (apl/× 2.0 3.0)))
  (is (float= -5.5 (apl/× 11.0 -0.5)))
  (is (float= 0.0 (apl/× 11.0 0.0)))
  (is (every? true? (map float= [4.0 6.0 8.0 10.0] (apl/× 2.0 [2.0 3.0 4.0 5.0]))))
  (is (every? true? (map float= [4.0 9.0 16.0] (apl/× [2.0 3.0 4.0] [2.0 3.0 4.0])))))

(deftest division
  (is (float= 2.0 (apl/÷ 4.0 2.0)))
  (is (float= 2.0 (apl/÷ 6.0 3.0)))
  (is (float= 11.0 (apl/÷ -5.5 -0.5)))
  (is (float= 11.0 (apl/÷ 11.0 1.0)))
  (is (every? true? (map float= [1.0 2.0 3.0 4.0] (apl/÷ 12.0 [12 6 4 3]))))
  (is (every? true? (map float= [1.0 3.0 2.0 3.0] (apl/÷ [12.0 18.0 8.0 9.0] [12 6 4 3]))))
  (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/÷ 1 0))))

(deftest maximum
  (is (= 2.0 (apl/⌈ 2.0 1.0)))
  (is (= 2.0 (apl/⌈ 2.0 2.0)))
  (is (= 2.0 (apl/⌈ 2.0 -3.0)))
  (is (= -3.0 (apl/⌈ -7.0 -3.0)))
  (is (= [0.0 0.0 0.0 1.0 2.0] (apl/⌈ 0.0 [-2.0 -1.0 0.0 1.0 2.0])))
  (is (= [2.0 1.0 0.0 1.0 2.0] (apl/⌈ [2.0 1.0 0.0 -1.0 -2.0] [-2.0 -1.0 0.0 1.0 2.0]))))

(deftest take-vector
  (is (= [] (apl/↑ 0 [1 2 3 4 5])))
  (testing "taking from the left"
    (is (= [1] (apl/↑ 1 [1 2 3 4 5])))
    (is (= [1 2 3] (apl/↑ 3 [1 2 3 4 5]))))
  (testing "taking from the right"
    (is (= [5] (apl/↑ -1 [1 2 3 4 5])))
    (is (= [4 5] (apl/↑ -2 [1 2 3 4 5]))))
  (testing "zero padding"
    (is (= [1 2 3 4 5 0 0 0] (apl/↑ 8 [1 2 3 4 5])))
    (is (= [0 0 0 0 0] (apl/↑ 5 [])))
    (is (= [0 0 0 1 2 3 4 5] (apl/↑ -8 [1 2 3 4 5])))
    (is (= [0 0 0 0 0] (apl/↑ -5 [])))))

(deftest drop-vector
  (is (= [1 2 3 4 5] (apl/↓ 0 [1 2 3 4 5])))
  (testing "dropping from the beginning"
    (is (= [4 5] (apl/↓ 3 [1 2 3 4 5])))
    (is (= [] (apl/↓ 5 [1 2 3 4 5])))
    (is (= [] (apl/↓ 6 [1 2 3 4 5]))))
  (testing "droppping from the end"
    (is (= [1 2 3 4] (apl/↓ -1 [1 2 3 4 5])))
    (is (= [1] (apl/↓ -4 [1 2 3 4 5])))
    (is (= [] (apl/↓ -5 [1 2 3 4 5])))
    (is (= [] (apl/↓ -6 [1 2 3 4 5])))
    (is (= [] (apl/↓ -8 [1 2 3 4 5])))))

(deftest reshape
  (testing "exact same number of elements"
    (is (= [1 2 3 4 5 6] (apl/⍴ [6] [1 2 3 4 5 6])))
    (is (= [1 2 3 4 5 6] (apl/⍴ [6] [[1 2] [3 4] [5 6]])))
    (is (= [[1 2 3] [4 5 6]] (apl/⍴ [2 3] [[1 2] [3 4] [5 6]])))
    (is (= [[1 2] [3 4] [5 6]] (apl/⍴ [3 2] [[1 2] [3 4] [5 6]])))
    (is (= [[[1] [2]] [[3] [4]] [[5] [6]]] (apl/⍴ [3 2 1] [[1 2] [3 4] [5 6]])))
    (is (= [[[1 2 3 4]] [[5 6 7 8]]] (apl/⍴ [2 1 4] [1 2 3 4 5 6 7 8]))))
  (testing "truncation"
    (is (= [1 2 3] (apl/⍴ [3] [[1 2] [3 4] [5 6]])))
    (is (= [[1] [2] [3] [4]] (apl/⍴ [4 1] [[1 2] [3 4] [5 6]])))
    (is (= [[[1 2]] [[3 4]]] (apl/⍴ [2 1 2] [[1 2] [3 4] [5 6]]))))
  (testing "zero-padding for empty vector"
    (is (= [0 0 0 0] (apl/⍴ [4] [])))
    (is (= [[0 0] [0 0] [0 0]] (apl/⍴ [3 2] [])))
    (is (= [[[0 0][0 0]] [[0 0][0 0]] [[0 0][0 0]]] (apl/⍴ [3 2 2] []))))
  (testing "repeating for larger target shape"
    (is (= [1 2 3 4 5 6 1 2] (apl/⍴ [8] [1 2 3 4 5 6])))
    (is (= [[1 2 3] [4 5 6] [7 1 2]] (apl/⍴ [3 3] [1 2 3 4 5 6 7])))
    (is (= [[[1 2 3 4]] [[5 1 2 3]]] (apl/⍴ [2 1 4] [1 2 3 4 5])))))
