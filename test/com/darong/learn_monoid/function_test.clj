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
