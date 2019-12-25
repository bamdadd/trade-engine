(ns exchange.core-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer [select]]
            [exchange.core :refer :all]))

(defn other-order [order]
  (if (= order :buy)
    :sell
    :buy))

(defn remove-once [pred coll]
  ((fn inner [coll]
     (lazy-seq
       (when-let [[x & xs] (seq coll)]
         (if (pred x)
           xs
           (cons x (inner xs))))))
   coll))

(defn execute-order [order-type size price order-book]
  (let [orders (get order-book order-type)
        other-orders (get order-book (other-order order-type))
        top-other-order (first (filter #(= (:price %) price) other-orders))
        _ (prn top-other-order)
        ]
    (cond (= (:size top-other-order) size)
          (do
            (prn order-type size order-book)
            (hash-map order-type orders
                      (other-order order-type)
                      (remove-once #(and (= (:price %) price)
                                         (= (:size %) size)) other-orders)))
          :else (assoc order-book
                  order-type
                  (conj orders {:size  size
                                :price price})))
    ))

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
  )
