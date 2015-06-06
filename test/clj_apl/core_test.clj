(ns clj-apl.core-test
  (:require [clojure.test :refer :all]
            [clj-apl.core :as apl]))

(defn float=
  "http://gettingclojure.wikidot.com/cookbook:numbers"
  ([x y] (float= x y 0.00001))
  ([x y epsilon]
     (let [scale (if (or (zero? x) (zero? y)) 1 (Math/abs x))]
       (<= (Math/abs (- x y)) (* scale epsilon)))) )

(deftest monadic-fns
  (testing "reciprocal"
    (is (= 0.5 (apl/÷ 2.0)))
    (is (= 0.25 (apl/÷ 4.0)))
    (is (= 0.125 (apl/÷ 8.0))))
  (testing "not"
    (is (= 1 (apl/∼ 0)))
    (is (= 0 (apl/∼ 1)))
    (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∼ 5)))
    (is (thrown-with-msg? ArithmeticException #"Domain error" (apl/∼ -1))))
  (testing "powers of euler's constant"
    (is (= 1.0 (apl/⋆ 0.0)))
    (is (= Math/E (apl/⋆ 1.0)))
    (is (float= 0.36787944117144233 (apl/⋆ -1.0)))
    (is (float= 735.0951892419727 (apl/⋆ 6.6))))
  (testing "natural log"
    (is (= 0.0 (apl/⍟ 1.0)))
    (is (float= 2.0149030205422647 (apl/⍟ 7.5)))
    (is (= 1.0 (apl/⍟ Math/E))))
  (testing "sign function"
    (is (= 1.0 (apl/× 1.0)))
    (is (= 1.0 (apl/× 234.4)))
    (is (= -1.0 (apl/× -564.2)))
    (is (= -1.0 (apl/× -1.2)))
    (is (= 0.0 (apl/× 0.0))))
  (testing "identity"
    (is (= 1.0 (apl/+ 1.0)))
    (is (= 234.4 (apl/+ 234.4)))
    (is (= -564.2 (apl/+ -564.2)))
    (is (= -1.2 (apl/+ -1.2)))
    (is (= 0.0 (apl/+ 0.0)))))
