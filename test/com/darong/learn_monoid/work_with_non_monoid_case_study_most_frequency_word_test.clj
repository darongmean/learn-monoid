(ns com.darong.learn-monoid.work-with-non-monoid-case-study-most-frequency-word-test
  "We found that most-frequent-word wasn’t a monoid homomorphism.

   The insight here is again to delay the calculation until the last minute, just as we did in
   the 'average' example.

   See https://fsharpforfunandprofit.com/posts/monoids-part3/
  "
  (:require
    [clojure.test :refer :all]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [clojure.test.check.generators :as gen]
    [clojure.spec.alpha :as spec]))

(defn add-text [s1 s2]
  (str s1 " " s2))

(defn most-frequent-word [text]
  (->> text
       (re-seq #"\w+")
       (frequencies)
       (sort-by second >)
       first
       first))

(deftest most-frequent-word-test
  (testing "check result"
    (is (= "a" (most-frequent-word "a b a")))
    (is (= "world" (most-frequent-word "hello world goodbye world"))))
  (testing "it is not monoid homomorphism"
    (let [page1 (reduce add-text (repeat 1 "hello world "))
          page2 (reduce add-text (repeat 1 "goodbye world "))
          page3 (reduce add-text (repeat 1 "foobar "))
          document [page1 page2 page3]]
      (is (= "world"
             (->> document (reduce add-text) (most-frequent-word))))
      (is (= "hello goodbye foobar"
             (->> document (map most-frequent-word) (reduce add-text))))
      (is (not= (->> document (reduce add-text) (most-frequent-word))
                (->> document (map most-frequent-word) (reduce add-text)))))))

;;; Rather than calculating the most frequent word upfront then, we create a data structure that
;;; stores all the information that we need to calculate the most frequent word later.

(defn word-freq [text]
  (->> text
       (re-seq #"\w+")
       (frequencies)))

(defn add-map [m1 m2]
  (merge-with + m1 m2))

(defn extract-most-frequent-word [m]
  (->> m
       (sort-by second >)
       first
       first))

(defn most-frequent-word-01 [& doc]
  (->> doc
       (map word-freq)
       (reduce add-map)
       (extract-most-frequent-word)))

(defn most-frequent-word-02 [& doc]
  (->> doc
       (reduce add-text)
       (word-freq)
       (extract-most-frequent-word)))

(deftest most-frequent-word-monoid-test
  (testing "check result most-frequent-word-01"
    (is (= "a" (most-frequent-word-01 "a b a")))
    (is (= "world" (most-frequent-word-01 "hello world goodbye world")))
    (is (= "0" (apply most-frequent-word-01 ["0" "0"]))))
  (testing "check result most-frequent-word-02"
    (is (= "a" (most-frequent-word-02 "a b a")))
    (is (= "world" (most-frequent-word-02 "hello world goodbye world")))
    (is (= "0" (apply most-frequent-word-02 ["0" "0"]))))
  (checking "add-map is closed" 100
    [x (gen/map gen/string gen/small-integer)
     y (gen/map gen/string gen/small-integer)]
    (is (spec/valid? (spec/map-of string? integer?) (add-map x y))))
  (checking "add-map is associative" 100
    [x (gen/map gen/string gen/small-integer)
     y (gen/map gen/string gen/small-integer)
     z (gen/map gen/string gen/small-integer)]
    (is (= (add-map (add-map x y) z)
           (add-map x (add-map y z)))))
  (checking "add-map has identity value" 100
    [x (gen/map gen/string gen/small-integer)]
    (is (= x (add-map x {}))
        (= x (add-map {} x))))
  (testing "word-freq is monoid homomorphism"
    (let [page1 (reduce add-text (repeat 1 "hello world "))
          page2 (reduce add-text (repeat 1 "goodbye world "))
          page3 (reduce add-text (repeat 1 "foobar "))
          document [page1 page2 page3]]
      (is (= "world" (apply most-frequent-word-01 document)))
      (is (= "world" (apply most-frequent-word-02 document)))))
  (checking "most-frequent-word-01 and most-frequent-word-02 have same result" 100
    [x (gen/vector gen/string-alphanumeric 1 1000)]
    (is (= (apply most-frequent-word-01 x)
           (apply most-frequent-word-02 x)))))
