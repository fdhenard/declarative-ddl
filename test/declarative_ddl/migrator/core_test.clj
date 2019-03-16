(ns declarative-ddl.migrator.core-test
  (:require [clojure.test :refer :all]
            [declarative-ddl.migrator.core :refer :all]
            [declarative-ddl.entities-schemas :as entities-schemas]
            [clojure.spec.alpha :as spec]
            [diff-as-list.core :as dal]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(deftest initial-migration-test
  (testing "initial migration"
    (let [the-diff {:keys-missing-in-1 [],
                    :keys-missing-in-2 [],
                    :value-differences
                    [{:path [],
                      :val-1 nil,
                      :val-2
                      {:cledgers-user
                       {:name "cledgers-user",
                        :fields
                        {:username
                         {:name "username",
                          :type :character,
                          :max-length 30,
                          :unique true},
                         :first-name
                         {:name "first-name", :type :character, :max-length 30},
                         :last-name {:name "last-name", :type :character, :max-length 30},
                         :email {:name "email", :type :character, :max-length 30},
                         :is-admin {:name "is-admin", :type :boolean, :default false},
                         :last-login {:name "last-login", :type :date-time, :null true},
                         :is-active {:name "is-active", :type :boolean, :default false},
                         :pass {:name "pass", :type :character, :max-length 300}}}}}],
                    :dal-version "2.2.6"}
          actual (diff-to-ddl the-diff)
          ;; _ (println "actual:" actual)
          ]
      (is (= actual
             (str/join
              "\n"
              ["CREATE TABLE cledgers_user ("
               "    id SERIAL PRIMARY KEY,"
               "    username VARCHAR(30) UNIQUE NOT NULL,"
               "    first_name VARCHAR(30) NOT NULL,"
               "    last_name VARCHAR(30) NOT NULL,"
               "    email VARCHAR(30) NOT NULL,"
               "    is_admin BOOLEAN NOT NULL DEFAULT(false),"
               "    last_login TIMESTAMP WITH TIME ZONE NULL,"
               "    is_active BOOLEAN NOT NULL DEFAULT(false),"
               "    pass VARCHAR(300) NOT NULL"
               ");"]))))))

(deftest add-a-table-test
  (testing "add a table"
    (let [the-diff {:keys-missing-in-1
                    [{:path [:xaction],
                      :value
                      {:name "xaction",
                       :fields
                       {:description
                        {:name "description", :type :character, :max-length 250},
                        :amount
                        {:name "amount",
                         :type :numeric,
                         :total-length 10,
                         :decimal-places 2},
                        :date {:name "date", :type :date},
                        :time-created
                        {:name "time-created", :type :date-time, :default :current-time},
                        :created-by
                        {:name "created-by",
                         :type :foreign-key,
                         :references :cledgers-user}}}}],
                    :keys-missing-in-2 [],
                    :value-differences [],
                    :dal-version "2.2.6"}
          actual (diff-to-ddl the-diff)
          ;; _ (println "actual:" actual)
          ]
      (is (= actual
             (str/join
              "\n"
              ["CREATE TABLE xaction ("
               "    id SERIAL PRIMARY KEY,"
               "    description VARCHAR(250) NOT NULL,"
               "    amount NUMERIC(10, 2) NOT NULL,"
               "    date DATE NOT NULL,"
               "    time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT(CURRENT_TIMESTAMP),"
               "    created_by_id INTEGER REFERENCES cledgers_user NOT NULL"
               ");"]))))))

(deftest alter-table-test
  (testing "alter table"
    (let [ents-before [{:name "test-add-remove-fields"
                        :fields [{:name "bye-bye"
                                  :type :int}]}
                       {:name "test-drop-table"
                        :fields [{:name "wont-appear-in-ddl"
                                  :type :int}]}]
          ents-after [{:name "test-add-remove-fields"
                       :fields [{:name "new-field-1"
                                 :type :int}
                                {:name "new-field-2"
                                 :type :int}]}
                      {:name "test-add-table"
                       :fields [{:name "new-table-field"
                                 :type :int}]}]
          ents-before-xformed (as-> ents-before $
                                (spec/assert ::entities-schemas/entities $)
                                (xform-entities-for-diff $))
          ents-after-xformed (as-> ents-after $
                               (spec/assert ::entities-schemas/entities $)
                               (xform-entities-for-diff $))
          the-diff (dal/diffl ents-before-xformed ents-after-xformed)
          actual (diff-to-ddl the-diff)
          ;; _ (println (str
          ;;             "actual type = " (type actual) "\n"
          ;;             "actual:\n" actual))
          expected (->> ["CREATE TABLE test_add_table ("
                         "    id SERIAL PRIMARY KEY,"
                         "    new_table_field INTEGER NOT NULL"
                         ");"
                         "DROP TABLE test_drop_table;"
                         "ALTER TABLE test_add_remove_fields"
                         "    ADD COLUMN new_field_1 INTEGER NOT NULL,"
                         "    ADD COLUMN new_field_2 INTEGER NOT NULL,"
                         "    DROP COLUMN bye_bye;"]
                        (interpose "\n")
                        (apply str))
          ;; _ (pp/pprint actual)
          ]
      (is (= actual expected)))))