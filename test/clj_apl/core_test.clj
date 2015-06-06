(ns clj-apl.core-test
  (:require [clojure.test :refer :all]
            [clj-apl.core :refer :all]))

(deftest monadic-fns
  (testing "reciprocal"
    (is (= 0.5 (divide 2.0)))
    (is (= 0.25 (divide 4.0)))
    (is (= 0.125 (divide 8.0)))))
