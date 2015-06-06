(ns clj-apl.core-test
  (:require [clojure.test :refer :all]
            [clj-apl.core :refer :all]))

(defn float=
  "http://gettingclojure.wikidot.com/cookbook:numbers"
  ([x y] (float= x y 0.00001))
  ([x y epsilon]
     (let [scale (if (or (zero? x) (zero? y)) 1 (Math/abs x))]
       (<= (Math/abs (- x y)) (* scale epsilon)))) )

(deftest monadic-fns
  (testing "reciprocal"
    (is (= 0.5 (divide 2.0)))
    (is (= 0.25 (divide 4.0)))
    (is (= 0.125 (divide 8.0))))
  (testing "not"
    (is (= 1 (logical-not 0)))
    (is (= 0 (logical-not 1)))
    (is (thrown-with-msg? ArithmeticException #"Domain error" (logical-not 5)))
    (is (thrown-with-msg? ArithmeticException #"Domain error" (logical-not -1))))
  (testing "powers of euler's constant"
    (is (= 1.0 (power 0.0)))
    (is (= Math/E (power 1.0)))
    (is (float= 0.36787944117144233 (power -1.0)))
    (is (float= 735.0951892419727 (power 6.6)))))
