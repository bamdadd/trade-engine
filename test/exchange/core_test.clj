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
            (execute-order :buy 1 100 :A order-book)
            {:buy  [{:size    1
                     :price   100
                     :account :A}]
             :sell []}))))
  (testing "sell"
    (let [order-book {:buy  []
                      :sell []}]
      (is (=
            (execute-order :sell 1 100 :A order-book)
            {:buy  []
             :sell [{:size    1
                     :price   100
                     :account :A}]}))))
  (testing "second-buy"
    (let [order-book {:buy  [{:size    1
                              :price   200
                              :account :A}]
                      :sell []}]
      (is (=
            (execute-order :buy 1 100 :B order-book)
            {:buy  [{:size    1
                     :price   200
                     :account :A} {:size    1
                                   :price   100
                                   :account :B}]
             :sell []}))))
  (testing "trade"

    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}]}]
      (is (=
            (execute-order :buy 1 100 :B order-book)
            {:buy  []
             :sell []}))))

  (testing "trade with history"
    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}
                             {:size    1
                              :price   100
                              :account :B}]}]
      (is (=
            (execute-order :buy 1 100 :C order-book)
            {:buy  []
             :sell [{:size    1
                     :price   100
                     :account :B}]}))))

  (testing "trade smaller"
    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}
                             {:size    1
                              :price   100
                              :account :B}]}]
      (is (=
            (execute-order :buy 0.5 100 :C order-book)
            {:buy  []
             :sell [{:size    0.5
                     :price   100
                     :account :A}
                    {:size    1
                     :price   100
                     :account :B}]}))))

  (testing "trade bigger"
    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}
                             {:size    1
                              :price   100
                              :account :B}]}]
      (is (=
            (execute-order :buy 1.5 100 :C order-book)
            {:buy  []
             :sell [{:size    0.5
                     :price   100
                     :account :B}]}))))

  (testing "buy bigger"
    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}
                             {:size    1
                              :price   100
                              :account :B}]}]
      (is (=
            (execute-order :buy 2.5 100 :C order-book)
            {:buy  [{:size    0.5
                     :price   100
                     :account :C}]
             :sell []}))))

  (testing "trade with threading"
    (let [order-book {:buy  []
                      :sell []}]
      (is (=
            (->> order-book
                 (execute-order :buy 1 100 :A)
                 (execute-order :sell 1 100 :B))
            {:buy  []
             :sell []}))))
  )
