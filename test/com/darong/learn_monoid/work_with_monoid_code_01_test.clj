(ns com.darong.learn-monoid.work-with-monoid-code-01-test
  "Some hacky implementation
   See https://fsharpforfunandprofit.com/posts/monoids-part2/
  "
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as string]
    [clojure.test :refer :all]
    [com.darong.learn-monoid.spec :as model]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]))


(def order-lines [{:ProductCode "AAA" :Qty 2 :Total 1998}
                  {:ProductCode "BBB" :Qty 1 :Total 199}
                  {:ProductCode "CCC" :Qty 3 :Total 399}])


(defn calculate-order-total-by-fold [order-lines]
  (let [accumulate-total (fn [acc {:keys [Total]}]
                           (+ acc Total))]
    (reduce accumulate-total 0 order-lines)))

(deftest calculate-order-total-by-fold-test
  (is (spec/valid? (spec/coll-of ::model/OrderLine) order-lines))
  (is (= 2596 (calculate-order-total-by-fold order-lines))))


;;; So far, so good. Now let’s look at a solution using a monoid approach.
;;; The addition must return a value of the same type!
;;;
;;; At first, this might seem like extra work, and just to add up a total. But note that we now have
;;; more information than just the total; we also have the sum of the qtys as well.

(defn add-line-semigroup [order-line1 order-line2]
  {:ProductCode "Total"
   :Qty (+ (:Qty order-line1) (:Qty order-line2))
   :Total (+ (:Total order-line1) (:Total order-line2))})

;;; There is a third requirement for a monoid – the zero or identity element.
;;; A problem would arise if we had an empty list of lines and we wanted to total them.
;;; What should the result be?

(defn add-line-monoid [order-line1 order-line2]
  ;; This does seem a bit hacky, so I wouldn’t recommend this technique in general.
  ;; There’s another way to get an identity that we’ll be discussing later.
  (cond
    (string/blank? (:ProductCode order-line1)) order-line2
    (string/blank? (:ProductCode order-line2)) order-line1
    :else
    {:ProductCode "Total"
     :Qty (+ (:Qty order-line1) (:Qty order-line2))
     :Total (+ (:Total order-line1) (:Total order-line2))}))

(defn calculate-order-total-semigroup [order-lines]
  (reduce add-line-semigroup order-lines))

(defn calculate-order-total-monoid [order-lines]
  (reduce add-line-monoid order-lines))

(deftest calculate-order-total-semigroup-test
  (testing "checking total result"
    (is (= 2596 (:Total (calculate-order-total-semigroup order-lines)))))
  (checking "it is closed" 100
    [x model/gen-order-line
     y model/gen-order-line]
    (is (spec/valid? ::model/OrderLine (add-line-semigroup x y))))
  (checking "it is associative" 100
    [x model/gen-order-line
     y model/gen-order-line
     z model/gen-order-line]
    (is (= (add-line-semigroup (add-line-semigroup x y) z)
           (add-line-semigroup x (add-line-semigroup y z))))))

(deftest calculate-order-total-monoid-test
  (testing "checking total result"
    (is (= 2596 (:Total (calculate-order-total-monoid order-lines)))))
  (checking "it is closed" 100
    [x model/gen-order-line
     y model/gen-order-line]
    (is (spec/valid? ::model/OrderLine (add-line-monoid x y))))
  (checking "it is associative" 100
    [x model/gen-order-line
     y model/gen-order-line
     z model/gen-order-line]
    (is (= (add-line-monoid (add-line-monoid x y) z)
           (add-line-monoid x (add-line-monoid y z)))))
  (checking "it has identity value" 100
    [x (gen/such-that #(-> % :ProductCode string/blank? not) model/gen-order-line)] ; hack to make it work
    (is (= x (add-line-monoid x {:ProductCode "" :Qty 0 :Total 0}))
        (= x (add-line-monoid {:ProductCode "" :Qty 0 :Total 0} x)))))
