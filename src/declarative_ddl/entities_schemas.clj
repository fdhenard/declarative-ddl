(ns declarative-ddl.entities-schemas
  (:require [clojure.spec.alpha :as spec]))


(spec/def :field/name string?)
;; (spec/def :field/type (spec/and keyword? #{:character :int :foreign-key}))
(spec/def :field/type #{:character :int :foreign-key})
;; (spec/def :field/type keyword?)

(defmulti field-type :type)
;; (defmulti field-type (fn [x]
;;                        (do
;;                          (println "what what:" x)
;;                          (:type x))))

(defmethod field-type :character [_]
  (spec/keys :req-un [:field/type :field/name :field/max-length]))
(defmethod field-type :foreign-key [_]
  (spec/keys :req-un [:field/type :field/name :field/references]))
(defmethod field-type :int [_]
  (spec/keys :req-un [:field/type :field/name]))




;; (spec/def ::field (spec/keys :req-un [:field/name :field/type]))
(spec/def ::field (spec/multi-spec field-type :field/type))

(def test-val {:type :int :name "what" :max-length 20})
