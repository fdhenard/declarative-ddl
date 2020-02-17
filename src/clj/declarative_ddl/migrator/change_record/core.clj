(ns declarative-ddl.migrator.change-record.core
  (:require [clojure.spec.alpha :as spec]
            [camel-snake-kebab.core :as csk]
            [diff-as-list.core :as dal]
            [declarative-ddl.entities.core :as entities]
            [declarative-ddl.clj-utils.core :as clj-utils]))


(defn diff-to-change-record-type [diff]
  (let [diff-path (:path diff)]
    (cond
      (and (instance? diff_as_list.core.KeyMissingDiff diff)
           (= 1 (count diff-path)))
      ::TableAddDrop
      (and (instance? diff_as_list.core.ValueDiff diff)
           (empty? diff-path)
           (nil? (:val-1 diff)))
      ::MultiTableAddDrop
      (and (instance? diff_as_list.core.KeyMissingDiff diff)
           (= 3 (count diff-path))
           (= :fields (get diff-path 1)))
      ::FieldAddDrop
      (and (instance? diff_as_list.core.KeyMissingDiff diff)
           (= 4 (count diff-path))
           (= :unique (get diff-path 3))
           (:value diff))
      ::UniqueConstraintAddDrop
      (and ;; (instance? diff_as_list.core.KeyMissingDiff diff)
           (= 4 (count diff-path))
           (= :null (get diff-path 3)))
      ::NotNullConstraintAddDrop
      :else (throw (RuntimeException.
                    "this case has not been implemented")))))




(spec/def ::path (spec/coll-of keyword? :kind vector?))


(defmulti diff-value class)
(defmethod diff-value ::TableAddDrop [_]
  :declarative-ddl.entities.core/entity)
(defmethod diff-value ::FieldAddDrop [_]
  :declarative-ddl.entities.core/field)


(spec/def ::value (spec/multi-spec diff-value ::change-record-type))
(spec/def ::diff (spec/keys :req-un [::path ::value]))
(spec/def ::change-record (spec/keys :req-un [::diff]))


;; ::ChangeRecord methods

(defmulti get-table-name-kw class)
(defmethod get-table-name-kw ::ChangeRecord [dadg]
  (-> dadg :diff :path first))

;; defined for all children of ::ChangeRecord
(defmulti get-ddl-from-change-rec (fn [ddl-genable _]
                                    (class ddl-genable)))




(defprotocol DdlAddDropGenable
  (get-ddl-add [this])
  (get-ddl-drop [this]))

(derive ::DdlAddDropGenable ::ChangeRecord)

 ;; defined for all ::DdlAddDropGenable
(defmulti get-group class)

;; defined for all ::DdlAddDropGenable
(defmulti get-missing-in-1->forward-add-or-drop class)
(defmethod get-missing-in-1->forward-add-or-drop :default [_]
  :add)

;; method for all ::ChangeRecord
(defmethod get-ddl-from-change-rec ::DdlAddDropGenable [dadg forward-or-backward]
  (let [#_ (clojure.pprint/pprint dadg)
        reverse-add-drop {:add :drop
                          :drop :add}
        dadg-diff (:diff dadg)
        missing-in (cond
                     (instance? diff_as_list.core.ValueDiff dadg-diff)
                     (let [val-1 (:val-1 dadg-diff)
                           val-2 (:val-2 dadg-diff)]
                       (cond
                         (and (nil? val-1)
                              (not (nil? val-2)))
                         :one
                         (and (nil? (:val-2 dadg-diff))
                              (not (nil? val-1)))
                         :two
                         (and (boolean? val-1) (boolean? val-2))
                         (cond
                           val-2 :one
                           val-1 :two
                           :else (throw (RuntimeException. "should not be here")))
                         :else (throw (RuntimeException. "should not be here"))))
                     (instance? diff_as_list.core.KeyMissingDiff dadg-diff)
                     (:missing-in dadg-diff)
                     :else (throw (RuntimeException. "should not be here")))
        missing-in-1->forward-add-or-drop (get-missing-in-1->forward-add-or-drop dadg)
        missing-in-fwd-bwd->add-or-drop
        {[:one :forward] missing-in-1->forward-add-or-drop
         [:two :backward] missing-in-1->forward-add-or-drop
         [:one :backward] (reverse-add-drop missing-in-1->forward-add-or-drop)
         [:two :forward] (reverse-add-drop missing-in-1->forward-add-or-drop)}
        add-or-drop (missing-in-fwd-bwd->add-or-drop [missing-in forward-or-backward])
        _ (when (nil? add-or-drop)
            (throw (RuntimeException. (str "invalid missing-in and forward-or-backward "
                                                    "; missing-in = " missing-in
                                                    ", forward-or-backward = " forward-or-backward))))
        action-map {:add get-ddl-add
                    :drop get-ddl-drop}
        action-fn (get action-map add-or-drop)
        _ (when (nil? action-fn)
            (throw (RuntimeException. (str "can't find action for add-or-drop = " add-or-drop))))]
    (action-fn dadg)))

(defprotocol OtherDdlGenable
  (get-ddl [this forward-or-backward]))




(derive ::OtherDdlGenable ::ChangeRecord)

(defmethod get-ddl-from-change-rec ::OtherDdlGenable [odg forward-or-backward]
  #_(println "odg:" odg ", forward-or-backward:" forward-or-backward)
  (get-ddl odg forward-or-backward))




(derive ::TableAddDrop ::DdlAddDropGenable)

;; method for ::DdlAddDropGenable
(defmethod get-group ::TableAddDrop [tad]
  :top-level)

;; defined for ::TableAddDrop
(defmulti get-table-definition class)
(defmethod get-table-definition ::TableAddDrop [tad]
  (-> tad :diff :value))


(defn table-name-kw->ddl [kw-in]
  (-> kw-in name csk/->snake_case))

(defmulti get-table-ddl-name class)
(defmethod get-table-ddl-name ::TableAddDrop [tad]
  (-> tad get-table-definition :name csk/->snake_case))





(derive ::MultiTableAddDrop ::DdlAddDropGenable)

;; method for ::DdlAddDropGenable
(defmethod get-group ::MultiTableAddDrop [mtad]
  :top-level)

(defmulti get-table-definitions class)
(defmethod get-table-definitions ::MultiTableAddDrop [mtad]
  (-> mtad :diff :val-2 vals))

(defmulti get-table-ddl-names class)
(defmethod get-table-ddl-names ::MultiTableAddDrop [mtad]
  (map #(-> % :name csk/->snake_case) (get-table-definitions mtad)))



(derive ::ConstraintAddDrop ::DdlAddDropGenable)




(derive ::FieldAddDrop ::DdlAddDropGenable)

;; method for ::DdlAddDropGenable
(defmethod get-group ::FieldAddDrop [fad]
  [::AlterTable (get-table-name-kw fad)])

(defmulti get-field-definition class)
(defmethod get-field-definition ::FieldAddDrop [fad]
  (-> fad :diff :value))

(defmulti get-field-ddl-name class)
(defmethod get-field-ddl-name ::FieldAddDrop [fad]
  (-> fad get-field-definition entities/get-field-ddl-name))
(defmethod get-field-ddl-name ::ConstraintAddDrop [cad]
  (-> cad :diff :path (get 2) name csk/->snake_case))



(derive ::UniqueConstraintAddDrop ::ConstraintAddDrop)

;; method for ::DdlAddDropGenable
(defmethod get-group ::ConstraintAddDrop [cad]
  [::AlterTable (get-table-name-kw cad)])

(defmulti get-unique-constraint-ddl-name class)
(defmethod get-unique-constraint-ddl-name ::UniqueConstraintAddDrop [ucad]
  (let [ddl-table-name (-> ucad get-table-name-kw table-name-kw->ddl)]
    (str ddl-table-name "_" (get-field-ddl-name ucad) "_unique")))



(derive ::NotNullConstraintAddDrop ::ConstraintAddDrop)

;; method for ::DdlAddDropGenable
(defmethod get-missing-in-1->forward-add-or-drop ::NotNullConstraintAddDrop [_]
  :drop)




(derive ::AlterTable ::OtherDdlGenable)
