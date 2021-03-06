(ns declarative-ddl.migrator.change-record.factories
  (:require [declarative-ddl.migrator.change-record.postgres :as postgres]
            [declarative-ddl.migrator.change-record.core :as change-record]))



(defn make-change-record [diff db-dialect]
  (let [change-rec-type (change-record/diff-to-change-record-type diff)
        constructor-map
        {[:postgres ::change-record/TableAddDrop]
         postgres/->PostgresTableAddDrop
         [:postgres ::change-record/FieldAddDrop]
         postgres/->PostgresFieldAddDrop
         [:postgres ::change-record/UniqueConstraintAddDrop]
         postgres/->PostgresUniqueConstraintAddDrop
         [:postgres ::change-record/NotNullConstraintAddDrop]
         postgres/->PostgresNotNullConstraintAddDrop
         [:postgres ::change-record/MultiTableAddDrop]
         postgres/->PostgresMultiTableAddDrop}
        constructor-func (get constructor-map [db-dialect change-rec-type])
        _ (when-not constructor-func
            (throw (ex-info
                    "could not find constructor-func for db-dialect, and change record type"
                    {:db-dialect db-dialect
                     :change-record-type change-rec-type})))]
    (constructor-func diff)))


(defn make-grouped-change-record [[grouping change-records] db-dialect]
  (let [group-category (first grouping)
        constructor-map
        {[:postgres ::change-record/AlterTable] postgres/->PostgresAlterTable}
        constructor (get constructor-map [db-dialect group-category])
        _ (when-not constructor
            (throw (ex-info
                    "could not find a constructor for dialect, and group category"
                    {:db-dialect db-dialect
                     :group-category group-category})))]
    (constructor (second grouping) change-records)))
