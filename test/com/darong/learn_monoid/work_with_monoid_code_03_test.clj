(ns com.darong.learn-monoid.work-with-monoid-code-03-test
  "Map to different structure, aka MapReduce

   It seems to be a good fit with Clojure idioms.

   See https://fsharpforfunandprofit.com/posts/monoids-part2/
  "
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.test :refer :all]
    [com.darong.learn-monoid.spec :as model]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [clojure.spec.alpha :as s])
  (:import
    (java.time LocalDate Duration)))

(def now (LocalDate/now))

(def customers [{:Name "Alice" :LastActive (.minusDays now 3) :TotalSpend 100}
                {:Name "Bob" :LastActive (.minusDays now 4) :TotalSpend 45}
                {:Name "Charlie" :LastActive (.minusDays now 5) :TotalSpend 42}])


(defn to-stats [{:keys [LastActive TotalSpend] :as _customer}]
  {:TotalInactiveDays (-> (Duration/between (.atStartOfDay LastActive) (.atStartOfDay now))
                          (.toDays))
   :Count 1
   :TotalSpend TotalSpend})


(defn add-line-monoid [stats1 stats2]
  {:TotalInactiveDays (+ (:TotalInactiveDays stats1) (:TotalInactiveDays stats2))
   :Count (+ (:Count stats1) (:Count stats2))
   :TotalSpend (+ (:TotalSpend stats1) (:TotalSpend stats2))})


(defn aggregate-customer-stats-monoid [customers]
  (->> customers
       (map to-stats)
       (reduce add-line-monoid)))

(deftest ^:focus aggregate-customer-stats-monoid-test
  (testing "checking customer type"
    (is (s/valid? (spec/coll-of ::model/Customer) customers)))
  (checking "to-stats has no error" 100
    [x model/gen-customer]
    (is (s/valid? ::model/CustomerStats (to-stats x))))
  (testing "checking total result"
    (is (= {:Count 3 :TotalInactiveDays 12 :TotalSpend 187} (aggregate-customer-stats-monoid customers))))
  (checking "it is closed" 100
    [x model/gen-customer-stats
     y model/gen-customer-stats]
    (is (spec/valid? ::model/CustomerStats (add-line-monoid x y))))
  (checking "it is associative" 100
    [x model/gen-customer-stats
     y model/gen-customer-stats
     z model/gen-customer-stats]
    (is (= (add-line-monoid (add-line-monoid x y) z)
           (add-line-monoid x (add-line-monoid y z)))))
  (checking "it has identity value" 100
    [x model/gen-customer-stats]
    (is (= x (add-line-monoid x {:TotalInactiveDays 0 :Count 0 :TotalSpend 0}))
        (= x (add-line-monoid {:TotalInactiveDays 0 :Count 0 :TotalSpend 0} x)))))
