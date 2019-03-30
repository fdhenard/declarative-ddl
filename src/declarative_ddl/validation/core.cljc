(ns declarative-ddl.validation.core
  (:require [clojure.spec.alpha :as spec]))



(defn field-def-to-spec [field-definition]
  (let [field-type (:type field-definition)]
   (case field-type
     :character
     (let [is-below-max-len? (fn [x] (<= (count x) (:max-length field-definition)))
           not-nil-spec (spec/and string?
                                  is-below-max-len?)]
       (if (:null field-definition)
         (spec/or :nil nil?
                  :not-nil not-nil-spec)
         not-nil-spec))
     :int
     (if (:null field-definition)
       (spec/nilable int?)
       int?)
     :numeric
     (let [is-precision-below-bounds? (fn [x]
                            (let [dec-val (bigdec x)]
                              (<= (.precision dec-val) (:total-length field-definition))))
           is-scale-below-bounds? (fn [x]
                        (let [dec-val (bigdec x)]
                          (<= (.scale dec-val) (:decimal-places field-definition))))
           will-coerce-to-bigdec? (fn [x] (try
                                            (bigdec x)
                                            true
                                            (catch java.lang.NumberFormatException nfe
                                              false)))
           not-nil-spec (spec/and will-coerce-to-bigdec?
                                  is-precision-below-bounds?
                                  is-scale-below-bounds?)]
       (if (:null field-definition)
         (spec/or :nil nil?
                  :not-nil not-nil-spec)
         not-nil-spec))
     (throw (Exception. (str "no validation defined for " field-type))))))
