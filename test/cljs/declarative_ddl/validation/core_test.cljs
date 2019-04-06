(ns declarative-ddl.validation.core-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [declarative-ddl.validation.core :as val]
            [clojure.spec.alpha :as spec]))

(def numeric-field-def {:type :numeric
                        :total-length 5
                        :decimal-places 2})

(def numeric-field-spec (val/field-def-to-spec numeric-field-def))

(deftest numeric-too-many-decimal-places
  (let [splain (spec/explain-data numeric-field-spec 1.111)
        problem-str (-> splain :cljs.spec.alpha/problems first :pred str)]
    (is (= problem-str "is-decimal-places-below-bounds?"))))

(deftest numeric-ok
  (let [splain (spec/explain-data numeric-field-spec 1.11)]
    (is (nil? splain))))

(deftest numeric-too-big
  (let [splain (spec/explain-data numeric-field-spec 11111.1)
        problem-str (-> splain :cljs.spec.alpha/problems first :pred str)]
    (is (= "is-total-length-below-bounds?" problem-str))))

(deftest numeric-not-numeric
  (let [splain (spec/explain-data numeric-field-spec "a")
        problem-str (-> splain :cljs.spec.alpha/problems first :pred str)]
    (is (= "declarative-ddl.validation.core/will-coerce-to-decimal?" problem-str))))

(deftest numeric-empty-str
  (let [splain (spec/explain-data numeric-field-spec "")
        problem-str (-> splain :cljs.spec.alpha/problems first :pred str)]
    (is (= "declarative-ddl.validation.core/will-coerce-to-decimal?" problem-str))))

(deftest numeric-ok-dec-as-str
  (let [splain (spec/explain-data numeric-field-spec "1.11")]
    (is nil? splain)))

(deftest numeric-nil
  (let [splain (spec/explain-data numeric-field-spec nil)
        problem-str (-> splain :cljs.spec.alpha/problems first :pred str)]
    (is (= "declarative-ddl.validation.core/will-coerce-to-decimal?" problem-str))))

(def numeric-field-nilable-def {:type :numeric
                                :total-length 5
                                :decimal-places 2
                                :null true})
(def numeric-field-nilable-spec (val/field-def-to-spec numeric-field-nilable-def))

(deftest numeric-ok-nil
  (let [splain (spec/explain-data numeric-field-nilable-spec nil)]
    (is nil? splain)))
