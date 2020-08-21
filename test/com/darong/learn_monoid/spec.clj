(ns com.darong.learn-monoid.spec
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.test.check.generators :as gen])
  (:import
    (java.time LocalDate)
    (java.time.temporal ChronoField)))

(spec/def ::ProductCode string?)
(spec/def ::Qty (spec/with-gen integer? (fn [] gen/nat)))
(spec/def ::Total (spec/with-gen integer? (fn [] gen/nat)))
(spec/def ::OrderLine (spec/keys :req-un [::ProductCode ::Qty ::Total]))

(def gen-order-line (spec/gen ::OrderLine))

(spec/def ::Price ::Total)
(spec/def ::LineTotal ::Total)
(spec/def ::OrderTotal ::Total)
(spec/def ::ProductLine (spec/keys :req-un [::ProductCode ::Qty ::LineTotal ::Price]))
(spec/def ::TotalLine (spec/keys :req-un [::Qty ::OrderTotal]))
(spec/def ::EmptyOrder #{{}})
(spec/def ::OrderLine2 (spec/or :product ::ProductLine :total ::TotalLine :empty ::EmptyOrder))

(def gen-order-line2 (spec/gen ::OrderLine2))


(def gen-local-date
  (let [day-range (.range (ChronoField/EPOCH_DAY))
        day-min (.getMinimum day-range)
        day-max (.getMaximum day-range)]
    (gen/fmap #(LocalDate/ofEpochDay %)
              (gen/large-integer* {:min day-min :max day-max}))))

(defn local-date? [x]
  (instance? LocalDate x))

(spec/def ::Name (spec/with-gen string? (fn [] gen/string-ascii)))
(spec/def ::Count (spec/with-gen integer? (fn [] gen/nat)))
(spec/def ::LastActive (spec/with-gen local-date? (fn [] gen-local-date)))
(spec/def ::TotalSpend (spec/with-gen integer? (fn [] gen/nat)))
(spec/def ::TotalInactiveDays (spec/with-gen integer? (fn [] gen/nat)))
(spec/def ::Customer (spec/keys :req-un [::Name ::LastActive ::TotalSpend]))
(spec/def ::CustomerStats (spec/keys :req-un [::Count ::TotalInactiveDays ::TotalSpend]))

(def gen-customer (spec/gen ::Customer))
(def gen-customer-stats (spec/gen ::CustomerStats))
