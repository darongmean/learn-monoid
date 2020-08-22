(ns com.darong.learn-monoid.work-with-non-monoid-associative-test
  "Associativity

   The trick in many cases is to move the operation into a property of each element.
   Make the operation a noun, rather than a verb.

   We can see that 5 - (3 - 2) is not equal to (5 - 3) - 2. But, 3 - 2 can be thought of as 3 + (-2).
   Rather than 'subtraction' as a verb, we have 'negative 2' as a noun. In that case, the example
   becomes 5 + (-3) + (-2). And since we are now using addition as the operator, we do have associativity,
   and 5 + (-3 + -2) is indeed the same as (5 + -3) + -2.

   A similar approach works with division. 12 / 3 / 2 can be converted into 12 * (1/3) * (1/2), and
   now we are back to multiplication as the operator, which is associative.

   DESIGN TIP:
   - To get associativity for an operation, try to move the operation into the object.

   See https://fsharpforfunandprofit.com/posts/monoids-part3/
  "
  (:require
    [clojure.test :refer :all]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [clojure.test.check.generators :as gen]
    [clojure.spec.alpha :as spec]))

(defn subtract-char [coll chars]
  (apply str (remove #((set chars) %) coll)))

(deftest subtract-char-not-monoid
  (checking "it is closed" 100
    [x gen/string
     y gen/string]
    (is (string? (subtract-char x y))))
  (testing "it is not associative"
    (let [x "3"
          y ""
          z "3"]
      (is (not= (subtract-char (subtract-char x y) z)
                (subtract-char x (subtract-char y z)))))))

;;; The trick is move the 'subtract-ness' from the operator into the object, just as we did with
;;; the numbers. What I mean is that we replace the plain strings with a 'subtract' or 'chars to remove'
;;; data structure that captures what we want to remove. And then we 'apply' the data structure to the string.
;;;
;;; The way to think about this approach is that, in a sense, we are modelling actions rather than data.
;;; We have a list of CharsToRemove actions, then we combine them into a single 'big' CharsToRemove action,
;;; and then we execute that single action at the end, after we have finished the intermediate manipulations.

(spec/def ::CharsToRemove (spec/with-gen (spec/and (spec/coll-of char?) set?)
                                         (fn [] (gen/set gen/char))))

(defn subtract [s]
  (into #{} s))

(defn apply-to [s chars-to-remove]
  (apply str (remove chars-to-remove s)))

(defn combine-chars-to-remove [c1 c2]
  (clojure.set/union c1 c2))

(defn subtract-char-m [s1 & ss]
  (->> ss
       (map subtract)
       (reduce combine-chars-to-remove #{})
       (apply-to s1)))

(deftest combine-validation-test
  (testing "check result"
    (is (= "" (subtract-char-m "abcdef" "abc" "def"))))
  (checking "it should return same result with non monoid version" 100
    [x gen/string
     y gen/string]
    (is (= (subtract-char x y)
           (subtract-char-m x y))))
  (checking "it is closed" 100
    [x (spec/gen ::CharsToRemove)
     y (spec/gen ::CharsToRemove)]
    (is (spec/valid? ::CharsToRemove (combine-chars-to-remove x y))))
  (checking "it is associative" 100
    [x (spec/gen ::CharsToRemove)
     y (spec/gen ::CharsToRemove)
     z (spec/gen ::CharsToRemove)]
    (is (= (combine-chars-to-remove (combine-chars-to-remove x y) z)
           (combine-chars-to-remove x (combine-chars-to-remove y z)))))
  (checking "it has identity value" 100
    [x (spec/gen ::CharsToRemove)]
    (is (= x (combine-chars-to-remove x #{}))
        (= x (combine-chars-to-remove #{} x)))))

;;; In fact rather than creating this CharsToRemove data structure, we could have just
;;; partially applied the original subtract-char function. And now we don’t even need a special apply-to function.
;;;
;;; But when we have more than one of these subtraction functions, what do we do? Each of these
;;; partially applied functions has signature string -> string, so how can we 'add' them together?
;;;
;;; The answer is function composition, of course!

;; (Note that we reverse the parameters to make partial application easier)
(defn subtract-char-reverse [chars-to-remove s]
  (subtract-char s chars-to-remove))

(defn subtract-char-m-02 [s1 & ss]
  (let [removal-action (->> ss
                            (map #(partial subtract-char-reverse %))
                            (reduce comp identity))]
    (removal-action s1)))

(deftest combine-validation-2-test
  (testing "check result"
    (is (= "def" (subtract-char-m-02 "abcdef" "abc")))
    (is (= "" (subtract-char-m-02 "abcdef" "abc" "def"))))
  (checking "it should return same result with non monoid version" 100
    [x gen/string
     y gen/string]
    (is (= (subtract-char x y)
           (subtract-char-m-02 x y))))
  (checking "it should return same result with other monoid version" 100
    [x gen/string
     ys (gen/vector gen/string 0 (count x))]
    (is (= (apply subtract-char-m (cons x ys))
           (apply subtract-char-m-02 (cons x ys)))))
  (checking "it is closed" 100
    [x gen/string
     y gen/string]
    (let [f (comp (partial subtract-char-reverse y))]
      (is (string? (f x)))))
  (checking "it is associative" 100
    [s gen/string
     x gen/string
     y gen/string
     z gen/string]
    (let [f (partial subtract-char-reverse x)
          g (partial subtract-char-reverse y)
          h (partial subtract-char-reverse z)
          c1 (comp (comp f g) h)
          c2 (comp f (comp g h))]
      (is (= (c1 s)
             (c2 s)))))
  (checking "it has identity value" 100
    [s gen/string
     x gen/string]
    (let [f (partial subtract-char-reverse x)
          c1 (comp identity f)
          c2 (comp f identity)]
      (is (= (c1 s)
             (c2 s))))))

;;; The 'data structure as action' and function approach are not exactly the same – the CharsToRemove approach
;;; may be more efficient, for example, because it uses a set, and is only applied to strings at the end –
;;; but they both achieve the same goal. Which one is better depends on the particular problem you’re working on.
