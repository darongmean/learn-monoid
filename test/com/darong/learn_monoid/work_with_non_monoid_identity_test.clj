(ns com.darong.learn-monoid.work-with-non-monoid-identity-test
  "Identity

   DESIGN TIP:
   - To get identity for an operation, create a special case in a discriminated union, or,
     even simpler, just use Option.

   In Clojure, nil punning provides most of the functionality that Scala & Haskell get from Option & Maybe.
   There is some-> and some->> available since Clojure 1.5. The some-> function is providing the Option[T].
   Empty sequence could also be considered instead of nil.

   See https://fsharpforfunandprofit.com/posts/monoids-part3/
  "
  (:require
    [clojure.test :refer :all]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [clojure.spec.alpha :as spec]))

;;; Let’s also revisit the ValidationResult type that we described earlier when talking about using lists to get closure.
;;; So, rather than thinking of it as a “Result”, let’s think of the type as storing failures, and
;;; rewrite it like this instead, with the failure case first.

(spec/def ::SuccessType #{:Success})
(spec/def ::FailureM (spec/coll-of string?))
(spec/def ::FailureTypeM (spec/keys :req-un [::FailureM]))
(spec/def ::ValidationResultM (spec/or :success ::SuccessType :failure ::FailureTypeM))

;;; Yes! It’s the option type again! Can we never get away from the darn thing?
(spec/def ::ValidationFailure (spec/or :success #{[]} :failure (spec/coll-of string?)))

(defn failure [s]
  [s])

(defn validate-bad-word [bad-word name]
  (when (.contains name bad-word)
    (failure (str "string contains a bad word: " bad-word))))

(defn validate-length [max-length name]
  (when (< max-length (count name))
    (failure "string is too long")))

(defn combine-validation [v1 v2]
  (concat v1 v2))

(deftest combine-validation-test
  (testing "check result"
    (let [validate-fn (juxt (partial validate-length 10)
                            (partial validate-length 100)
                            (partial validate-bad-word "monad")
                            (partial validate-bad-word "java")
                            (partial validate-bad-word "cobol"))]
      (is (= ["string is too long"
              "string contains a bad word: monad"
              "string contains a bad word: cobol"]
             (->> "cobol has native support for monads"
                  (validate-fn)
                  (reduce combine-validation nil))))))
  (checking "it is closed" 100
    [x (spec/gen ::ValidationFailure)
     y (spec/gen ::ValidationFailure)]
    (is (spec/valid? ::ValidationFailure (combine-validation x y))))
  (checking "it is associative" 100
    [x (spec/gen ::ValidationFailure)
     y (spec/gen ::ValidationFailure)
     z (spec/gen ::ValidationFailure)]
    (is (= (combine-validation (combine-validation x y) z)
           (combine-validation x (combine-validation y z)))))
  (checking "it has identity value" 100
    [x (spec/gen ::ValidationFailure)]
    (is (= x (combine-validation x []))
        (= x (combine-validation [] x)))))

