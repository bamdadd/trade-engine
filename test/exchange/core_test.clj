(ns exchange.core-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer [select]]
            [exchange.core :refer :all]))

;TODO: add account

(defn opposite-order [order]
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
        opposite-orders (get order-book (opposite-order order-type))
        top-other-order (first (filter #(= (:price %) price) opposite-orders))
        ;_ (prn top-other-order)
        ]
    (cond
      (and
        (contains? top-other-order :size)
        (= (:size top-other-order) size))
      (do
        (println "Trade" order-type size price)
        (hash-map order-type orders
                  (opposite-order order-type)
                  (remove-once #(and (= (:price %) price)
                                     (= (:size %) size)) opposite-orders)))

      (and
        (contains? top-other-order :size)
        (< size (:size top-other-order))
        (= price (:price top-other-order)))
      (let [matched-item
            (first (filter #(= (:price %) price) opposite-orders))
            remaining-items
            (remove-once #(and (= (:price %) price)) opposite-orders)
            remaining-size (- (:size matched-item) size)
            new-item {:size remaining-size :price price}
            ]
        (println "partial trade" order-type size price)
        (hash-map order-type orders
                  (opposite-order order-type)
                  (conj remaining-items new-item)))

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
  )
