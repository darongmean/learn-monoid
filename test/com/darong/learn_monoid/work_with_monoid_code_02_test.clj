(ns com.darong.learn-monoid.work-with-monoid-code-02-test
  "Introducing a special total type and empty type.

   This seems to be more complex in Clojure. Better do this with static type like Haskell or F#.

   See https://fsharpforfunandprofit.com/posts/monoids-part2/
  "
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.test :refer :all]
    [com.darong.learn-monoid.spec :as model]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]))


(def order-lines [{:ProductCode "AAA" :Qty 2 :Price 999 :LineTotal 1998}
                  {:ProductCode "BBB" :Qty 1 :Price 199 :LineTotal 199}
                  {}])


(defn add-line-monoid [order-line1 order-line2]
  ;; Unfortunately, the addition logic is more complicated now,
  ;; as we have to handle every combination of cases:
  (cond
    (spec/valid? ::model/EmptyOrder order-line1) order-line2

    (spec/valid? ::model/EmptyOrder order-line2) order-line1

    (and (spec/valid? ::model/ProductLine order-line1) (spec/valid? ::model/ProductLine order-line2))
    {:Qty (+ (:Qty order-line1) (:Qty order-line2))
     :OrderTotal (+ (:LineTotal order-line1) (:LineTotal order-line2))}

    (and (spec/valid? ::model/ProductLine order-line1) (spec/valid? ::model/TotalLine order-line2))
    {:Qty (+ (:Qty order-line1) (:Qty order-line2))
     :OrderTotal (+ (:LineTotal order-line1) (:OrderTotal order-line2))}

    (and (spec/valid? ::model/TotalLine order-line1) (spec/valid? ::model/ProductLine order-line2))
    {:Qty (+ (:Qty order-line1) (:Qty order-line2))
     :OrderTotal (+ (:OrderTotal order-line1) (:LineTotal order-line2))}

    (and (spec/valid? ::model/TotalLine order-line1) (spec/valid? ::model/TotalLine order-line2))
    {:Qty (+ (:Qty order-line1) (:Qty order-line2))
     :OrderTotal (+ (:OrderTotal order-line1) (:OrderTotal order-line2))}))


(defn calculate-order-total-monoid [order-lines]
  (reduce add-line-monoid {} order-lines))

(deftest calculate-order-total-monoid-test
  (testing "checking total result"
    (is (= {:OrderTotal 2197, :Qty 3} (calculate-order-total-monoid order-lines))))
  (checking "it is closed" 100
    [x model/gen-order-line2
     y model/gen-order-line2]
    (is (spec/valid? ::model/OrderLine2 (add-line-monoid x y))))
  (checking "it is associative" 100
    [x model/gen-order-line2
     y model/gen-order-line2
     z model/gen-order-line2]
    (is (= (add-line-monoid (add-line-monoid x y) z)
           (add-line-monoid x (add-line-monoid y z)))))
  (checking "it has identity value" 100
    [x model/gen-order-line2]
    (is (= x (add-line-monoid x {}))
        (= x (add-line-monoid {} x)))))
