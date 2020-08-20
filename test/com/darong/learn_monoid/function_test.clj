(ns com.darong.learn-monoid.function-test
  "Monoids are not really a design pattern; more an approach to working with many different types
   of values in a common way.

   A monoid is not just “a bunch of things”, but “a bunch of things” and “some way of combining them”.
   So, for example, “the integers” is not a monoid, but “the integers under addition” is a monoid.

   A set of generalized requirements that can apply to all sorts of things:
   - You start with a bunch of things, and some way of combining them two at a time.
   - Rule 1 (Closure): The result of combining two things is always another one of the things.
   - Rule 2 (Associativity): When combining more than two things, which pairwise combination
     you do first doesn’t matter.
   - Rule 3 (Identity element): There is a special thing called “zero” such that when you combine
     any thing with “zero” you get the original thing back.

   See https://fsharpforfunandprofit.com/posts/monoids-without-tears/
  "
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.generators :as gen]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]))


;;; Integer


(deftest integer-under-addition-is-monoid
  (checking "it is closed" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (integer? (+ x y))))
  (checking "it is associative" 100
    [x gen/small-integer
     y gen/small-integer
     z gen/small-integer]
    (is (= (+ (+ x y) z) (+ x (+ y z)))))
  (checking "it has identity value 0" 100
    [x gen/small-integer]
    (is (= x (+ x 0) (+ 0 x)))))


(deftest integer-under-multiplication-is-monoid
  (checking "it is closed" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (integer? (* x y))))
  (checking "it is associative" 100
    [x gen/small-integer
     y gen/small-integer
     z gen/small-integer]
    (is (= (* (* x y) z) (* x (* y z)))))
  (checking "it has identity value 1" 100
    [x gen/small-integer]
    (is (= x (* x 1) (* 1 x)))))


(deftest integer-under-subtraction-is-not-monoid
  (checking "it is closed" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (integer? (- x y))))
  (checking "it is not associative" 100
    [x (gen/such-that #(not= 0 %) gen/small-integer) ; 0 is special case but not identity value
     y (gen/such-that #(not= 0 %) gen/small-integer)
     z (gen/such-that #(not= 0 %) gen/small-integer)]
    (is (not= (- (- x y) z) (- x (- y z))))))


(deftest integer-under-max-is-monoid
  (checking "it is closed" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (integer? (max x y))))
  (checking "it is associative" 100
    [x gen/small-integer
     y gen/small-integer
     z gen/small-integer]
    (is (= (max (max x y) z) (max x (max y z)))))
  (checking "it has identity value Integer/MIN_VALUE" 100
    [x gen/small-integer]
    (is (= x (max x Integer/MIN_VALUE) (max Integer/MIN_VALUE x)))))


(deftest integer-under-equality-is-not-monoid
  (checking "it is not closed" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (not (integer? (= x y))))))


(deftest integer-under-less-than-is-not-monoid
  (checking "it is not closed" 100
    [x gen/small-integer
     y gen/small-integer]
    (is (not (integer? (< x y))))))


;;; Float


(deftest float-under-multiplication-is-not-monoid
  (checking "it is closed" 100
    [x gen/double
     y gen/double]
    (is (double? (* x y))))
  (testing "it is not associative"
    (let [x 1.5
          y 1.2313690185548012
          z 1.751953125]
      (is (not= (* (* x y) z) (* x (* y z))))))
  (checking "it has identity value 1" 100
    [x (gen/double* {:NaN? false})]
    (is (= x (* x 1) (* 1 x)))))


(deftest float-under-division-is-not-monoid
  (checking "it is closed" 100
    [x (gen/such-that #(not= 0.0 %) gen/double)
     y (gen/such-that #(not= 0.0 %) gen/double)]
    (is (double? (/ x y))))
  (testing "it is not associative"
    (let [x 1.0
          y 1.0
          z 0.5]
      (is (not= (/ (/ x y) z) (/ x (/ y z)))))))


;; Positive Number


(deftest positive-number-under-addition-is-semigroup
  (let [positive-number (gen/such-that pos? gen/nat)]
    (checking "it is closed" 100
      [x positive-number
       y positive-number]
      (is (integer? (+ x y))))
    (checking "it is associative" 100
      [x positive-number
       y positive-number
       z positive-number]
      (is (= (+ (+ x y) z) (+ x (+ y z)))))
    (checking "it has no identity value" 100
      [x positive-number
       y positive-number]
      (is (not= x (+ x y) (+ y x))))))


(deftest positive-number-under-multiplication-is-monoid
  (let [positive-number (gen/such-that pos? gen/nat)]
    (checking "it is closed" 100
      [x positive-number
       y positive-number]
      (is (integer? (* x y))))
    (checking "it is associative" 100
      [x positive-number
       y positive-number
       z positive-number]
      (is (= (* (* x y) z) (* x (* y z)))))
    (checking "it has identity value 1" 100
      [x positive-number]
      (is (= x (* x 1) (* 1 x))))))


;;; Boolean


(deftest boolean-under-and-is-monoid
  (checking "it is closed" 100
    [x gen/boolean
     y gen/boolean]
    (is (boolean? (and x y))))
  (checking "it is associative" 100
    [x gen/boolean
     y gen/boolean
     z gen/boolean]
    (is (= (and (and x y) z) (and x (and y z)))))
  (checking "it has identity value true" 100
    [x gen/boolean]
    (is (= x (and x true) (and true x)))))


(deftest boolean-under-or-is-monoid
  (checking "it is closed" 100
    [x gen/boolean
     y gen/boolean]
    (is (boolean? (or x y))))
  (checking "it is associative" 100
    [x gen/boolean
     y gen/boolean
     z gen/boolean]
    (is (= (or (or x y) z) (or x (or y z)))))
  (checking "it has identity value false" 100
    [x gen/boolean]
    (is (= x (or x false) (or false x)))))


;;; String


(deftest string-under-concatenation-is-monoid
  (checking "it is closed" 100
    [x gen/string
     y gen/string]
    (is (string? (str x y))))
  (checking "it is associative" 100
    [x gen/string
     y gen/string
     z gen/string]
    (is (= (str (str x y) z) (str x (str y z)))))
  (checking "it has identity value empty string" 100
    [x gen/string]
    (is (= x (str x "") (str "" x)))))


(deftest string-under-equality-is-not-monoid
  (checking "it is not closed" 100
    [x gen/string
     y gen/string]
    (is (not (string? (= x y))))))


(deftest string-under-subtract-chars-is-not-monoid
  (let [subtract-char (fn [coll chars]
                        (apply str (remove #((set chars) %) coll)))]
    (checking "it is closed" 100
      [x gen/string
       y gen/string]
      (is (string? (subtract-char x y))))
    (testing "it is not associative"
      (let [x "3"
            y ""
            z "3"]
        (is (not= (subtract-char (subtract-char x y) z)
                  (subtract-char x (subtract-char y z))))))))


;;; List


(deftest list-under-concatenation-is-monoid
  (checking "it is closed" 100
    [x (gen/vector gen/any)
     y (gen/vector gen/any)]
    (is (seq? (concat x y))))
  (checking "it is associative" 100
    [x (gen/vector gen/any)
     y (gen/vector gen/any)
     z (gen/vector gen/any)]
    (is (= (concat (concat x y) z) (concat x (concat y z)))))
  (checking "it has identity value empty list" 100
    [x (gen/vector gen/any)]
    (is (= x (concat x []) (concat [] x)))))


(deftest list-under-intersection-is-semigroup
  (checking "it is closed" 100
    [x (gen/set gen/any)
     y (gen/set gen/any)]
    (is (set? (clojure.set/intersection x y))))
  (checking "it is associative" 100
    [x (gen/set gen/any)
     y (gen/set gen/any)
     z (gen/set gen/any)]
    (is (= (clojure.set/intersection (clojure.set/intersection x y) z)
           (clojure.set/intersection x (clojure.set/intersection y z)))))
  (testing "it has not identity value"
    (let [x #{0}]
      (is (not= x (clojure.set/intersection x #{}))))))
