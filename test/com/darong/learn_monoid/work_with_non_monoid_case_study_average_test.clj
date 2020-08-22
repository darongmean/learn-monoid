(ns com.darong.learn-monoid.work-with-non-monoid-case-study-average-test
  "So now we have the toolkit that will enable us to deal with the thorny case of averages.

   First, it is not closed.
   Second, even if it was closed, avg is not associative.
   Finally, there is no identity.

   See https://fsharpforfunandprofit.com/posts/monoids-part3/
  "
  (:require
    [clojure.test :refer :all]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [clojure.test.check.generators :as gen]
    [clojure.spec.alpha :as spec]))

(defn avg [i j]
  (double (/ (+ i j) 2.0)))

(deftest avg-not-monoid-test
  (checking "it is not closed" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (not (integer? (avg x y)))))
  (testing "it is not associative"
    (is (not= (avg (avg 1 2) 3)
              (avg 1 (avg 2 3))))))

;;; So let’s apply the design tips to see if they help us come up with a solution.
;;;
;;; 1. To easily create a monoidal type, make sure that each field of the type is also a monoid.
;;; Well, “average” is a mathematical operation, so we could expect that a monoidal equivalent
;;; would also be based on numbers.
;;;
;;; 2. To enable closure for a non-numeric type, replace single items with lists.
;;; This looks at first glance like it won’t be relevant, so we’ll skip this for now.
;;;
;;; 3. To get associativity for an operation, try to move the operation into the object.
;;; Here’s the crux! How do we convert “average” from a verb (an operation) to a noun (a data structure)?
;;;
;;; The answer is that we create a structure that is not actually an average, but
;;; a “delayed average” – everything you need to make an average on demand.
;;;
;;; That is, we need a data structure with two components: a total, and a count.
;;; With these two numbers we can calculate an average as needed.

(spec/def ::Total (spec/with-gen integer? (fn [] gen/small-integer)))
(spec/def ::Count (spec/with-gen integer? (fn [] gen/small-integer)))
(spec/def ::Avg (spec/keys :req-un [::Total ::Count]))

(defn add-avg [v1 v2]
  (merge-with + v1 v2))

;;; The last tip is:
;;; - To get identity for an operation, create a special case in a discriminated union, or, even simpler, just use Option.
;;; In this case, the tip is not needed, as we can easily create a zero by setting the two components to be zero:

(def zero {:Total 0 :Count 0})

(deftest add-avg-test
  (checking "it is closed" 100
    [x (spec/gen ::Avg)
     y (spec/gen ::Avg)]
    (is (spec/valid? ::Avg (add-avg x y))))
  (checking "it is associative" 100
    [x (spec/gen ::Avg)
     y (spec/gen ::Avg)
     z (spec/gen ::Avg)]
    (is (= (add-avg (add-avg x y) z)
           (add-avg x (add-avg y z)))))
  (checking "it has identity value" 100
    [x (spec/gen ::Avg)]
    (is (= x (add-avg x zero))
        (= x (add-avg zero x)))))

(defn make-avg [n]
  {:Total n :Count 1})

(defn calc-avg [{:keys [Total Count] :as avg}]
  (if (= zero avg)
    0
    (double (/ Total Count))))

(defn avg-02 [& coll]
  (->> coll
       (map make-avg)
       (reduce add-avg zero)
       (calc-avg)))

(deftest avg-02-test
  (checking "it is should return same result with naive version" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (= (avg x y)
           (avg-02 x y))))
  (testing "check result"
    (is (= 5.5 (apply avg-02 (range 1 11))))))
