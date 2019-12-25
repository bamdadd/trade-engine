(ns exchange.core-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer [select]]
            [exchange.core :refer :all]
            [exchange.trade-engine :refer [execute-order]]))

(deftest order-book-test
  (testing "buy"
    (let [order-book {:buy  []
                      :sell []}]
      (is (=
            (execute-order :buy 1 100 order-book)
            {:buy  [{:size  1
                     :price 100}]
             :sell []}))))
  (testing "sell"
    (let [order-book {:buy  []
                      :sell []}]
      (is (=
            (execute-order :sell 1 100 order-book)
            {:buy  []
             :sell [{:size  1
                     :price 100}]}))))
  (testing "second-buy"
    (let [order-book {:buy  [{:size  1
                              :price 200}]
                      :sell []}]
      (is (=
            (execute-order :buy 1 100 order-book)
            {:buy  [{:size  1
                     :price 200} {:size  1
                                  :price 100}]
             :sell []}))))
  (testing "trade"
    (let [order-book {:buy  []
                      :sell [{:size  1
                              :price 100}]}]
      (is (=
            (execute-order :buy 1 100 order-book)
            {:buy  []
             :sell []}))))

  (testing "trade with history"
    (let [order-book {:buy  []
                      :sell [{:size  1
                              :price 100}
                             {:size  1
                              :price 100}]}]
      (is (=
            (execute-order :buy 1 100 order-book)
            {:buy  []
             :sell [{:size  1
                     :price 100}]}))))

  (testing "trade smaller"
    (let [order-book {:buy  []
                      :sell [{:size  1
                              :price 100}
                             {:size  1
                              :price 100}]}]
      (is (=
            (execute-order :buy 0.5 100 order-book)
            {:buy  []
             :sell [{:size  0.5
                     :price 100}
                    {:size  1
                     :price 100}]}))))

  (testing "trade bigger"
    (let [order-book {:buy  []
                      :sell [{:size  1
                              :price 100}
                             {:size  1
                              :price 100}]}]
      (is (=
            (execute-order :buy 1.5 100 order-book)
            {:buy  []
             :sell [{:size  0.5
                     :price 100}]}))))

  (testing "trade with threading"
    (let [order-book {:buy  []
                      :sell []}]
      (is (=
            (->> order-book
                 (execute-order :buy 1 100)
                 (execute-order :sell 1 100)
                 )
            {:buy  []
             :sell []}))))
  )
