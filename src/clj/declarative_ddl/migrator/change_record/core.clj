(ns declarative-ddl.migrator.change-record.core
  (:require [clojure.spec.alpha :as spec]
            [declarative-ddl.entities.core :as entities]
            [declarative-ddl.clj-utils.core :as clj-utils]))



(defn diff-to-change-record-type [diff]
  (let [diff-path (:path diff)]
    (cond
      (= 1 (count diff-path))
      ::TableAddDrop
      (and (= 3 (count diff-path))
           (= :fields (get diff-path 1)))
      ::FieldAddDrop
      (and (= 4 (count diff-path))
           (= :unique (get diff-path 3))
           (:value diff))
      ::UniqueConstraintAddDrop
      :else
      (throw (Exception. "this case has not been implemented - make-addition-ddl - size of path")))))




(spec/def ::path (spec/coll-of keyword? :kind vector?))


(defmulti diff-value class)
(defmethod diff-value ::TableAddDrop [_]
  :declarative-ddl.entities.core/entity)
(defmethod diff-value ::FieldAddDrop [_]
  :declarative-ddl.entities.core/field)


(spec/def ::value (spec/multi-spec diff-value ::change-record-type))
(spec/def ::diff (spec/keys :req-un [::path ::value]))
(spec/def ::change-record (spec/keys :req-un [::diff]))


(defmulti get-table-name-kw class)
(defmethod get-table-name-kw ::ChangeRecord [dadg]
  (-> dadg :diff :path first))


(defprotocol DdlAddDropGenable
  (get-ddl-add [this])
  (get-ddl-drop [this]))

(derive ::DdlAddDropGenable ::ChangeRecord)

(defprotocol OtherDdlGenable
  (get-ddl [this forward-or-backward]))

(derive ::OtherDdlGenable ::ChangeRecord)


(defmulti get-ddl-from-change-rec (fn [ddl-genable _]
                    (class ddl-genable)))
(defmethod get-ddl-from-change-rec ::DdlAddDropGenable [dadg forward-or-backward]
  (let [action-map
        {[:forward :one] get-ddl-add
         [:forward :two] get-ddl-drop
         [:backward :one] get-ddl-drop
         [:backward :two] get-ddl-add}
        missing-in (-> dadg :diff :missing-in)
        action-fn (get action-map [forward-or-backward missing-in])]
    (action-fn dadg)))
(defmethod get-ddl-from-change-rec ::OtherDdlGenable [odg forward-or-backward]
  (get-ddl odg forward-or-backward))


(derive ::TableAddDrop ::DdlAddDropGenable)

(defmulti get-table-definition class)
(defmethod get-table-definition ::TableAddDrop [tad]
  (-> tad :diff :value))


(defn table-name-kw->ddl [kw-in]
  (-> kw-in name clj-utils/undasherize))

(defmulti get-table-ddl-name class)
(defmethod get-table-ddl-name ::TableAddDrop [tad]
  (-> tad get-table-definition :name clj-utils/undasherize))

(defmulti get-group class)
(defmethod get-group ::TableAddDrop [tad]
  :top-level)
(defmethod get-group ::FieldAddDrop [fad]
  [::AlterTable (get-table-name-kw fad)])
(defmethod get-group ::UniqueConstraintAddDrop [ucad]
  [::AlterTable (get-table-name-kw ucad)])



(derive ::FieldAddDrop ::DdlAddDropGenable)

(defmulti get-field-definition class)
(defmethod get-field-definition ::FieldAddDrop [fad]
  (-> fad :diff :value))


(derive ::UniqueConstraintAddDrop ::DdlAddDropGenable)

(defmulti get-field-ddl-name class)
(defmethod get-field-ddl-name ::FieldAddDrop [fad]
  (-> fad get-field-definition entities/get-field-ddl-name))
(defmethod get-field-ddl-name ::UniqueConstraintAddDrop [ucad]
  (-> ucad :diff :path (get 2) name clj-utils/undasherize))


(defmulti get-unique-constraint-ddl-name class)
(defmethod get-unique-constraint-ddl-name ::UniqueConstraintAddDrop [ucad]
  (let [ddl-table-name (-> ucad get-table-name-kw name clj-utils/undasherize)]
    (str ddl-table-name "_" (get-field-ddl-name ucad) "_unique")))

(derive ::AlterTable ::OtherDdlGenable)
