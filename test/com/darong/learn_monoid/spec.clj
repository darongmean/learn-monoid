(ns com.darong.learn-monoid.spec
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.test.check.generators :as gen]))

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
