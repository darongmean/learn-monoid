(ns com.darong.learn-monoid.spec
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.test.check.generators :as gen]))

(spec/def ::ProductCode string?)
(spec/def ::Qty (spec/with-gen integer? (fn [] gen/nat)))
(spec/def ::Total (spec/with-gen integer? (fn [] gen/nat)))
(spec/def ::OrderLine (spec/keys :req-un [::ProductCode ::Qty ::Total]))

(def gen-order-line (spec/gen ::OrderLine))
