(ns com.darong.learn-monoid.work-with-monoid-code-04-test
  "Monoid Homomorphisms

   A monoid homomorphism is a transformation that preserves an essential 'monoidness', even if the
   'before' and 'after' objects are quite different.

   The advantage of the monoid homomorphism approach is that it is 'chunkable'. As a direct consequence
   of this chunkability, we get some of the benefits:
   1. it is incremental
   2. it is parallelizable. The work for each chunk can be done independently, on different cores or
      machines. Note that in practice, parallelism is much overrated. The chunkability into small pieces
      has a much greater effect on performance than parallelism itself.

   See https://fsharpforfunandprofit.com/posts/monoids-part2/
  "
  (:require
    [clojure.core.reducers :as reducers]
    [clojure.test :refer :all]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [clojure.test.check.generators :as gen]
    [clojure.string :as string]))


;;; Let’s say we can add two smaller text blocks to make a larger text block:


(defn add-text [s1 s2]
  (cond
    (= "" s1) s2
    (= "" s2) s1
    :else (str s1 " " s2)))

(deftest add-text-monoid-test
  (testing "checking result"
    (is (= "Hello world" (add-text "Hello" "world"))))
  (checking "it is closed" 100
    [x gen/string
     y gen/string]
    (is (string? (add-text x y))))
  (checking "it is associative" 100
    [x gen/string
     y gen/string
     z gen/string]
    (is (= (add-text (add-text x y) z)
           (add-text x (add-text y z)))))
  (checking "it has identity value" 100
    [x gen/string]
    (is (= x (add-text x ""))
        (= x (add-text "" x)))))


;;; Now let’s say we are writing a book and we want a word count to show how much we have written.


(defn word-count [s]
  (->> (string/split s #" +")
       (remove string/blank?)
       (count)))

(deftest word-count-test
  (testing "checking result"
    (is (= 0 (word-count "")))
    (is (= 2 (word-count "Hello world")))))


;;; So we are writing away, and now we have produced three pages of text.
;;; How do we calculate the word count for the complete document?
;;;
;;; Well, one way is to add the separate pages together to make a complete text block, and then apply
;;; the word-count function to that text block.
;;;
;;;  Page 1 + Page 2 + Page 3 = Complete Text => Complete Count
;;;
;;; But every time we finish a new page, we have to add all the text together and do the word count all over again.
;;; The second approach relies on the fact that integers (the counts) are themselves a monoid, and
;;; you can add them up to get the desired result.
;;;
;;; Page 1 => Count + Page 2 => Count + Page 3 => Count = Complete Count
;;;
;;; So the word-count function has transformed an aggregation over “pages” into an aggregation over “counts”.
;;;
;;; The big question now: is word-count a monoid homomorphism?
;;; Well, pages (text) and counts (integers) are both monoids, so it certainly transforms one monoid into another.
;;;
;;; But the more subtle condition is: does it preserve the “shape”? That is, does the adding of the counts
;;; give the same answer as the adding of the pages?

(defn word-count-via-add-text [xs]
  (->> xs (reduce add-text "") (word-count)))

;; incremental version
(defn word-count-via-add-map [xs]
  (->> xs (map word-count) (reduce +)))

;; parallel version
(defn word-count-via-add-map-parallel [xs]
  (->> (into [] xs) (reducers/map word-count) (reducers/fold +)))

(deftest word-count-monoid-homomorphism-test
  (testing "check add page to make a complete book"
    (is (= 0 (->> [""] word-count-via-add-text)))
    (is (= 1 (->> [" " "!"] word-count-via-add-text)))
    (is (= 2 (->> ["!" "!"] word-count-via-add-text)))
    (is (= 2 (->> ["!" " !"] word-count-via-add-text)))
    (is (= 0 (->> ["" "\t" ""] word-count-via-add-text))))
  (testing "check map count then add"
    (is (= 0 (->> [""] word-count-via-add-map)))
    (is (= 1 (->> [" " "!"] word-count-via-add-map)))
    (is (= 2 (->> ["!" "!"] word-count-via-add-map)))
    (is (= 2 (->> ["!" " !"] word-count-via-add-map)))
    (is (= 0 (->> ["" "\t" ""] word-count-via-add-map))))
  (checking "word count preserve the shape" {:num-tests 100 :seed 1598106679362}
    [xs (gen/vector gen/string)]
    (is (= (->> xs word-count-via-add-text)
           (->> xs word-count-via-add-map)))))

;;; In this case, the answer is yes. So word-count is a monoid homomorphism!

;;; Comparing word count implementations
;;;
;;; Here are the results for the different implementations running on my 4 core machine:
;;; "Time taken for reduce then count was: 4082.924316 msecs"
;;; "Time taken for map then reduce was: 141.049676 msecs"
;;; "Time taken for parallel map then reduce was: 64.31344 msecs"
;;;
;;; This is the key to why monoid homomorphisms are important – they enable a "divide and conquer"
;;; strategy that is both powerful and easy to implement.

(defmacro time-ms [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         dur# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (prn (str "Elapsed time: " dur# " msecs"))
     {:ret ret# :dur dur#}))

(deftest compare-word-count-implementation-test
  (let [page (->> (cycle ["hello"]) (take 1000) (reduce add-text))
        document (->> (cycle [page]) (take 1000))
        count-add (time-ms (word-count-via-add-text document))
        count-map (time-ms (word-count-via-add-map document))
        count-parallel (time-ms (word-count-via-add-map-parallel document))]
    (testing "they should return the same result"
      (is (= (:ret count-add)
             (:ret count-map)
             (:ret count-parallel))))
    (testing "parallel should be faster than map version and map version should be faster than add-text version"
      (is (< (:dur count-parallel)
             (:dur count-map)
             (:dur count-add))))))
