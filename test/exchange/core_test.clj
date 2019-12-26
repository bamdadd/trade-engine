(ns exchange.core-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer [select]]
            [exchange.core :refer :all]
            [exchange.trade-engine :refer [execute-order]]))

(deftest order-book-test
  (testing "buy"
    (let [order-book {:buy  []
                      :sell []}
          trade-book []
          books {:order-book order-book
                 :trade-book trade-book}]
      (is (=
            (execute-order :buy 1 100 :A books)
            {:order-book
                         {:buy  [{:size    1
                                  :price   100
                                  :account :A}]
                          :sell []}
             :trade-book []}))))
  (testing "sell"
    (let [order-book {:buy  []
                      :sell []}
          trade-book []
          books {:order-book order-book
                 :trade-book trade-book}]
      (is (=
            (execute-order :sell 1 100 :A books)
            {:order-book
                         {:buy  []
                          :sell [{:account :A
                                  :price   100
                                  :size    1}]}
             :trade-book []}))))
  (testing "second-buy"
    (let [order-book {:buy  [{:size    1
                              :price   200
                              :account :A}]
                      :sell []}
          trade-book []
          books {:order-book order-book
                 :trade-book trade-book}]
      (is (=
            (execute-order :buy 1 100 :B books)
            {:order-book {:buy  [{:size    1
                                  :price   200
                                  :account :A} {:size    1
                                                :price   100
                                                :account :B}]
                          :sell []}
             :trade-book []}))))
  (testing "trade"

    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}]}
          trade-book []
          books {:order-book order-book
                 :trade-book trade-book}]
      (is (=
            (execute-order :buy 1 100 :B books)
            {:order-book {:buy  []
                          :sell []}
             :trade-book []}))))

  (testing "trade with history"
    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}
                             {:size    1
                              :price   100
                              :account :B}]}
          trade-book []
          books {:order-book order-book
                 :trade-book trade-book}]
      (is (=
            (execute-order :buy 1 100 :C books)
            {:trade-book []
             :order-book
                         {:buy  []
                          :sell [{:size    1
                                  :price   100
                                  :account :B}]}}))))

  (testing "trade smaller"
    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}
                             {:size    1
                              :price   100
                              :account :B}]}
          trade-book []
          books {:order-book order-book
                 :trade-book trade-book}]
      (is (=
            (execute-order :buy 0.5 100 :C books)
            {:trade-book []
             :order-book
                         {:buy  []
                          :sell [{:size    0.5
                                  :price   100
                                  :account :A}
                                 {:size    1
                                  :price   100
                                  :account :B}]}}))))

  (testing "trade bigger"
    (let [order-book1 {:buy  []
                       :sell [{:size    1
                               :price   100
                               :account :A}
                              {:size    1
                               :price   100
                               :account :B}]}
          trade-book []
          books {:order-book order-book1
                 :trade-book trade-book}]
      (is (=
            (execute-order :buy 1.5 100 :C books)
            {:order-book {:buy  []
                          :sell [{:size    0.5
                                  :price   100
                                  :account :B}]}
             :trade-book []}))))

  (testing "buy bigger"
    (let [order-book {:buy  []
                      :sell [{:size    1
                              :price   100
                              :account :A}
                             {:size    1
                              :price   100
                              :account :B}]}
          trade-book []
          books {:order-book order-book
                 :trade-book trade-book}]
      (is (=
            (execute-order :buy 2.5 100 :C books)
            {:trade-book []
             :order-book
                         {:buy  [{:size    0.5
                                  :price   100
                                  :account :C}]
                          :sell []}}))))

  (testing "trade with threading"
    (let [order-book {:buy  []
                      :sell []}
          trade-book []
          books {:order-book order-book
                 :trade-book []}]
      (is (=
            (->> books
                 (execute-order :buy 1 100 :A)
                 (execute-order :sell 1 100 :B))
            {:trade-book []
             :order-book
                         {:buy  []
                          :sell []}}))
      )
    )
  )
