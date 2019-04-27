(ns declarative-ddl.cljc.core)

;; regarding (dissoc :choices) - postgres can do a check constraint on the data in
;; a character field, but for simplicity sake for now, I will ignore that feature
;; and leave it to the user of auto-admin to make that restriction
(defn xform-fields-for-diff [fields-vec]
  (reduce
   (fn [accum field]
     (let [new-field (-> field (dissoc :choices))]
       (assoc accum (keyword (:name field)) new-field)))
   {}
   fields-vec))

;; (spec/fdef xform-fields-to-pure-map
;;            :args (spec/cat :field-vec (spec/coll-of :entities-schemas/field)))



(defn xform-entities-for-diff [entities-vec]
  (reduce
   (fn [accum entity]
     (let [new-entity (-> entity
                          (assoc :fields (xform-fields-for-diff (:fields entity)))
                          (dissoc :repr))]
       (assoc accum (keyword (:name new-entity)) new-entity)))
   {}
   entities-vec))
