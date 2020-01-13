(ns declarative-ddl.entities.core
  (:require [clojure.spec.alpha :as spec]
            [camel-snake-kebab.core :as csk]))



(spec/def :field/name string?)
(spec/def :field/type #{:character :int :foreign-key :boolean :date-time :date :numeric})
(spec/def :field-boolean/default boolean?)
(spec/def :field-datetime/default #{:current-time})
(spec/def :field-int/default int?)
(spec/def :field-numeric/default number?)
(spec/def :field-character/default string?)
(spec/def :field/max-length pos-int?)
(spec/def :field/total-length pos-int?)
(spec/def :field/decimal-places pos-int?)
(spec/def :field/null boolean?)

(defmulti field-type :type)

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

(spec/def ::field (spec/multi-spec field-type :field/type))


(spec/def :entity/name string?)

(spec/def :entity/fields (spec/and (spec/coll-of ::field :kind vector?)
                                   #(> (count %) 0)))

(spec/def ::entity (spec/keys :req-un [:entity/name :entity/fields]))

(spec/def ::entities (spec/coll-of ::entity :kind vector?))


(defmulti get-field-ddl-name #(:type %))
(defmethod get-field-ddl-name :foreign-key [field-definition]
  (-> field-definition :name (str "-id") csk/->snake_case))
(defmethod get-field-ddl-name :default [field-definition]
  (-> field-definition :name csk/->snake_case))





(comment

  (def test-val [{:name "what"
                 :fields [{:type :character
                           :name "what"
                           :max-length 100}]
                 }
                {:name "what"
                 :fields [{:type :character
                           :name "why"
                           :max-length "none"}]
                 }]))
