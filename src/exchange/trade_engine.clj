(ns exchange.trade-engine)

;TODO: add trades-book
;TODO: add transaction id

(defn -opposite-order [order]
  (if (= order :buy)
    :sell
    :buy))

(defn -remove-once [pred coll]
  ((fn inner [coll]
     (lazy-seq
       (when-let [[x & xs] (seq coll)]
         (if (pred x)
           xs
           (cons x (inner xs))))))
   coll))

(defn -process-equal [price size opposite-orders]
  (-remove-once #(and (= (:price %) price)
                      (= (:size %) size)) opposite-orders))

(defn -process-smaller [order-type size price orders account opposite-orders]
  (let [matched-item
        (first (filter #(= (:price %) price) opposite-orders))
        remaining-items

        (-remove-once #(and (= (:price %) price)) opposite-orders)
        trade-account (if (neg? (- (:size matched-item)))
                        (:account matched-item)
                        account)
        remaining-size (Math/abs (- (:size matched-item) size))
        new-item {:size    remaining-size
                  :price   price
                  :account trade-account}
        ]
    (hash-map order-type orders
              (-opposite-order order-type)
              (conj remaining-items new-item))))

(defn execute-order [order-type size price account {:keys [order-book trade-book]
                                                    :as   books
                                                    :or   {order-book {:buy  []
                                                                       :sell []}
                                                           trade-book []}
                                                    }]
  (let [orders (get order-book order-type)
        opposite-orders (get order-book (-opposite-order order-type))
        top-other-order (first (filter #(= (:price %) price) opposite-orders))
        ]
    (cond
      (and
        (contains? top-other-order :size)
        (= (:size top-other-order) size))
      (do
        (println "Trade" order-type size price)
        (merge books {:order-book
                      (hash-map order-type orders
                                (-opposite-order order-type)
                                (-process-equal price size opposite-orders))}))
      ;smaller
      (and
        (contains? top-other-order :size)
        (< size (:size top-other-order))
        (= price (:price top-other-order)))
      (do
        (println "partial trade smaller" order-type size price)
        (merge books {:order-book
                      (-process-smaller order-type size price orders account opposite-orders)}))
      ;bigger
      (and
        (contains? top-other-order :size)
        (> size (:size top-other-order))
        (= price (:price top-other-order)))
      (let [remaining-items
            (-remove-once #(and (= (:price %) price)) opposite-orders)]
        (println "partial trade bigger" order-type size price account)
        (execute-order order-type
                       (- size (:size top-other-order))
                       price
                       account
                       (merge books
                              {
                               :order-book
                               (merge order-book
                                      (hash-map
                                        (-opposite-order order-type) remaining-items))}))
        )
      ; equal
      :else
      (do
        (merge books
               {:trade-book
                []
                :order-book
                (assoc order-book
                  order-type
                  (conj orders {:size    size
                                :price   price
                                :account account}))})))))
