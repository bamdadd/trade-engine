(ns exchange.core-test
  (:require [clojure.test :refer :all]
            [exchange.core :refer :all]))


(defn add-order [type size price order-book]
  (assoc order-book
    type
    (conj (get order-book type) [size price])))

(deftest order-book-test
  (testing "buy"
    (let [order-book {:buy  []
                      :sell []}]
      (is (=
            (add-order :buy 1 100 order-book)
            {:buy  [[1 100]]
             :sell []}))))
  (testing "sell"
    (let [order-book {:buy  []
                      :sell []}]
      (is (=
            (add-order :sell 1 100 order-book)
            {:buy  []
             :sell [[1 100]]}))))
  (testing "second-buy"
    (let [order-book {:buy  [[1 200]]
                      :sell []}]
      (is (=
            (add-order :buy 1 100 order-book)
            {:buy  [[1 200] [1 100]]
             :sell []})))))
