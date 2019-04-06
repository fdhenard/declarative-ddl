(ns declarative-ddl.entities-schemas
  (:require [clojure.spec.alpha :as spec]))


(spec/def :field/name string?)
;; (spec/def :field/type (spec/and keyword? #{:character :int :foreign-key}))
(spec/def :field/type #{:character :int :foreign-key :boolean :date-time :date :numeric})
;; (spec/def :field/type keyword?)
;; (spec/def :field/default #{boolean? :current-time})
(spec/def :field-boolean/default boolean?)
(spec/def :field-datetime/default #{:current-time})
(spec/def :field-int/default int?)
(spec/def :field-numeric/default number?)
(spec/def :field-character/default string?)
;; (spec/def :field/default (spec/or :default-boolean boolean?
;;                                   :default-datetime #{:current-time}))
(spec/def :field/max-length pos-int?)
(spec/def :field/total-length pos-int?)
(spec/def :field/decimal-places pos-int?)
(spec/def :field/null boolean?)

;; (spec/def :field/common (spec/keys :req-un [:field/type :field/name]
;;                                    :opt-un [:field/default]))


;; (defmulti field-type :type)

;; (defmethod field-type :character [_]
;;   (spec/merge :field/common
;;               (spec/keys :req-un [:field/max-length])))
;; (defmethod field-type :foreign-key [_]
;;   (spec/merge :field/common
;;               (spec/keys :req-un [:field/references])))
;; (defmethod field-type :int [_]
;;   (spec/merge :field/common
;;               (spec/keys :req-un [:field/type :field/name])))
;; (defmethod field-type :boolean [_]
;;   :field/common)

(defmulti field-type :type)
;; (defmulti field-type (fn [x]
;;                        (do
;;                          (println "what what:" x)
;;                          (:type x))))

(defmethod field-type :character [_]
  (spec/keys :req-un [:field/type :field/name :field/max-length]
             :opt-un [:field-character/default :field/null]))
(defmethod field-type :int [_]
  (spec/keys :req-un [:field/type :field/name]
             :opt-un [:field-int/default :field/null]))
(defmethod field-type :foreign-key [_]
  (spec/keys :req-un [:field/type :field/name :field/references]
             :opt-un [:field/null]))
(defmethod field-type :boolean [_]
  (spec/keys :req-un [:field/type :field/name]
             :opt-un [:field-boolean/default :field/null]))
(defmethod field-type :numeric [_]
  (spec/keys :req-un [:field/type :field/name :field/total-length :field/decimal-places]
             :opt-un [:field-numeric/default :field/null]))
(defmethod field-type :date [_]
  (spec/keys :req-un [:field/type :field/name]
             :opt-un [:field-datetime/default :field/null]))
(defmethod field-type :date-time [_]
  (spec/keys :req-un [:field/type :field/name]
             :opt-un [:field-datetime/default :field/null]))
;; (defmethod field-type :int [_]
;;   (spec/keys :req-un [:field/type :field/name]
;;              :opt-un [:field/default]))
;; (defmethod field-type :boolean [_]
;;   (spec/keys :req-un [:field/type :field/name]
;;              :opt-un [:field/default]))

(spec/def ::field (spec/multi-spec field-type :field/type))


;; (def test-val {:type :character :name "what" :max-lenggg 20})

(spec/def :entity/name string?)

(spec/def :entity/fields (spec/and (spec/coll-of ::field :kind vector?)
                                   #(> (count %) 0)))

(spec/def ::entity (spec/keys :req-un [:entity/name :entity/fields]))

(spec/def ::entities (spec/coll-of ::entity :kind vector?))

(def test-val [{:name "what"
                :fields [{:type :character
                          :name "what"
                          :max-length 100}]
                }
               {:name "what"
                :fields [{:type :character
                          :name "why"
                          :max-length "none"}]
                }])