(ns declarative-ddl.validation.core
  (:require [clojure.spec.alpha :as spec]
            #?(:cljs [decimal.core :as decimal])))

(defn will-coerce-to-decimal? [x]
  (if (nil? x)
    false
    #?(:clj (try
              (bigdec x)
              true
              (catch java.lang.NumberFormatException nfe
                false))
       :cljs (try
               (decimal/decimal x)
               true
               (catch js/Error e
                 (if (clojure.string/starts-with? (.-message e) "[DecimalError] Invalid argument:")
                   false
                   (throw e)))))))


(defn coerce-to-decimal [x]
  #?(:clj (bigdec x)
     :cljs (decimal/decimal x)))



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
     (let [is-total-length-below-bounds?
           (fn [x]
             (let [dec-val (coerce-to-decimal x)]
               (<= (.precision dec-val) (:total-length field-definition))))
           is-decimal-places-below-bounds?
           (fn [x]
             (let [dec-val (coerce-to-decimal x)
                   dec-places #?(:clj (.scale dec-val)
                                 :cljs (.decimalPlaces dec-val))]
               (<= dec-places (:decimal-places field-definition))))
           not-nil-spec (spec/and will-coerce-to-decimal?
                                  is-total-length-below-bounds?
                                  is-decimal-places-below-bounds?)]
       (if (:null field-definition)
         (spec/or :nil nil?
                  :not-nil not-nil-spec)
         not-nil-spec))
     (let [err-msg (str "no validation defined for " field-type)]
       (throw #?(:clj (Exception. err-msg)
                 :cljs (js/Error. err-msg)))))))
