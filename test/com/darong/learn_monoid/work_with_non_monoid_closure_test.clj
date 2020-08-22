(ns com.darong.learn-monoid.work-with-non-monoid-closure-test
  "Getting closure

   The type of the combined value is not the same as the type of the original values. How can you handle this?
   - One way is to just map from the original type to a new type that is closed. We saw this approach
     used with the Customer and CustomerStats.
   - Sometimes you really don’t want to use map, but instead want to design your type from the beginning
     so that it meets the closure requirement.

   DESIGN TIP:
   1. To easily create a monoidal type, make sure that each field of the type is also a monoid.
   2. To enable closure for a non-numeric type, replace single items with lists.
      And if lists aren’t performant enough for you, you can easily extend this approach to use
      classic data structures like trees, heaps, etc. or mutable types like ResizeArray.

   See https://fsharpforfunandprofit.com/posts/monoids-part3/
  "
  (:require
    [clojure.test :refer :all]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [clojure.test.check.generators :as gen]
    [clojure.spec.alpha :as spec]))


;;; 1. Combining closed types to make new compound types
;;;
;;; We’ve seen that
;;; - numeric types are closed under some basic math operations like addition and multiplication
;;; - some non-numeric types, like strings and lists, are closed under concatenation.
;;;
;;; With this in mind, it should be obvious that any combination of these types will be closed too.
;;; We just have to define the 'add' function to do the appropriate 'add' on the component types.
;;; This is exactly the approach we took with CustomerStats.


;;; 2. Dealing with non-numeric types


(defn add-char [c1 c2]
  {:pre [(char? c1) (char? c2)]}
  (str c1 c2))

(deftest add-char-test
  (checking "it is not closed" 100
    [x gen/char
     y gen/char]
    (is (not (char? (add-char x y))))))

;;; A string can be thought of as a list or array of chars.
;;; In other words, we could have used lists of chars instead, like this:

(spec/def ::MonoidalChar (spec/coll-of char?))

(defn add-monoidal-char [c1 c2]
  (concat c1 c2))

(deftest add-monoidal-char-test
  (testing "check result"
    (is (= [\a \b] (add-monoidal-char [\a] [\b]))))
  (checking "it is closed" 100
    [x (spec/gen ::MonoidalChar)
     y (spec/gen ::MonoidalChar)]
    (is (spec/valid? ::MonoidalChar (add-monoidal-char x y))))
  (checking "it is associative" 100
    [x (spec/gen ::MonoidalChar)
     y (spec/gen ::MonoidalChar)
     z (spec/gen ::MonoidalChar)]
    (is (= (add-monoidal-char (add-monoidal-char x y) z)
           (add-monoidal-char x (add-monoidal-char y z)))))
  (checking "it has identity value" 100
    [x (spec/gen ::MonoidalChar)]
    (is (= x (add-monoidal-char x []))
        (= x (add-monoidal-char [] x)))))

;;; In some cases, you might need to convert to a list when setting up the monoid
;;; and then convert to another type when you are done.

(defn to-mchar [c]
  [c])

(defn to-string [coll]
  (apply str coll))

(defn add-char-2 [c1 c2]
  (->> [c1 c2]
       (map to-mchar)
       (reduce add-monoidal-char)
       (to-string)))

(deftest add-char-compare-add-monoidal-char
  (checking "it return same result" 100
    [x gen/char
     y gen/char]
    (is (= (add-char x y)
           (add-char-2 x y)))))

;;; Another example, here is a simple module for doing some validation. There are two options, Success and Failure,
;;; and the Failure case also has a error string associated with it.

(spec/def ::SuccessType #{:Success})
(spec/def ::Failure string?)
(spec/def ::FailureType (spec/keys :req-un [::Failure]))
(spec/def ::ValidationResult (spec/or :success ::SuccessType :failure ::FailureType))

(defn validate-bad-word [bad-word name]
  (if (.contains name bad-word)
    {:Failure (str "string contains a bad word: " bad-word)}
    :Success))

(defn validate-length [max-length name]
  (if (< max-length (count name))
    {:Failure "string is too long"}
    :Success))

;;; In practice, we might perform multiple validations on a string, and we would like to return all
;;; the results at once, added together somehow.
;;; This calls out for being a monoid! If we can add two results pairwise, then we can extend the
;;; operation to add as many results as we like! So then the question is,
;;; how do we combine two validation results?
;;;
;;; A naive approach would be to concatenate the strings, but that wouldn’t work if we were using
;;; format strings, or resource ids with localization, etc. No, a better way is to convert the Failure case
;;; to use a list of strings instead of a single string. That will make combining results simple.

(spec/def ::FailureM (spec/coll-of string?))
(spec/def ::FailureTypeM (spec/keys :req-un [::FailureM]))
(spec/def ::ValidationResultM (spec/or :success ::SuccessType :failure ::FailureTypeM))

(defn failure [s]
  {:FailureM [s]})

(defn validate-bad-word-2 [bad-word name]
  (if (.contains name bad-word)
    (failure (str "string contains a bad word: " bad-word))
    :Success))

(defn validate-length-2 [max-length name]
  (if (< max-length (count name))
    (failure "string is too long")
    :Success))

(defn combine-validation [v1 v2]
  (cond
    (and (spec/valid? ::SuccessType v1)
         (spec/valid? ::SuccessType v2))
    :Success
    (and (spec/valid? ::SuccessType v1)
         (spec/valid? ::FailureTypeM v2))
    v2
    (and (spec/valid? ::FailureM v1)
         (spec/valid? ::SuccessType v2))
    v1
    (and (spec/valid? ::FailureTypeM v1)
         (spec/valid? ::FailureTypeM v1))
    {:FailureM (concat (:FailureM v1) (:FailureM v2))}))

(deftest combine-validation-test
  (testing "check result"
    (let [validate-fn (juxt (partial validate-length-2 10)
                            (partial validate-length-2 100)
                            (partial validate-bad-word-2 "monad")
                            (partial validate-bad-word-2 "java")
                            (partial validate-bad-word-2 "cobol"))]
      (is (= {:FailureM ["string is too long"
                         "string contains a bad word: monad"
                         "string contains a bad word: cobol"]}
             (->> "cobol has native support for monads"
                  (validate-fn)
                  (reduce combine-validation :Success))))))
  (checking "it is closed" 100
    [x (spec/gen ::ValidationResultM)
     y (spec/gen ::ValidationResultM)]
    (is (spec/valid? ::ValidationResultM (combine-validation x y))))
  (checking "it is associative" 100
    [x (spec/gen ::ValidationResultM)
     y (spec/gen ::ValidationResultM)
     z (spec/gen ::ValidationResultM)]
    (is (= (combine-validation (combine-validation x y) z)
           (combine-validation x (combine-validation y z)))))
  (checking "it has identity value" 100
    [x (spec/gen ::ValidationResultM)]
    (is (= x (combine-validation x :Success))
        (= x (combine-validation :Success x)))))
