(ns declarative-ddl.migrator.core-test
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [diff-as-list.core :as dal]
            [declarative-ddl.cljc.core :as dddl-cljc]
            [declarative-ddl.migrator.core :refer :all]
            [declarative-ddl.entities.core :as entities]
            [declarative-ddl.cljc.utils.core :as cljc-utils]))

(deftest initial-migration-test
  (testing "initial migration"
    (let [the-diff {:differences [(dal/make-value-diff
                                   {:path [],
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
                                       :pass {:name "pass", :type :character, :max-length 300}}}}})]
                    :dal-version "3.0.0-SNAPSHOT"}
          expected (str/join
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
                     ");"])
          actual (diff->ddl the-diff)
          ;; _ (println "actual:" actual)
          ]
      (is (= expected actual)))))

(deftest add-a-table-test
  (testing "add a table"
    (let [the-diff {:differences [(dal/make-key-missing-diff
                                   {:path [:xaction]
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
                                       :references :cledgers-user}}}
                                    :missing-in :one})]
                    :dal-version "3.0.0-SNAPSHOT"}
          actual (diff->ddl the-diff)
          ;; _ (println "actual:" actual)
          expected (str/join
              "\n"
              ["CREATE TABLE xaction ("
               "    id SERIAL PRIMARY KEY,"
               "    description VARCHAR(250) NOT NULL,"
               "    amount NUMERIC(10, 2) NOT NULL,"
               "    date DATE NOT NULL,"
               "    time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT(CURRENT_TIMESTAMP),"
               "    created_by_id INTEGER REFERENCES cledgers_user NOT NULL"
               ");"])]
      (is (= expected actual)))))

(defn xform-ents [ents]
  (as-> ents $
    (spec/assert ::entities/entities $)
    (dddl-cljc/xform-entities-for-diff $)))

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
          the-diff (dal/diffl (xform-ents ents-before) (xform-ents ents-after))
          actual (diff->ddl the-diff)
          #_ (println (str
                      "actual type = " (type actual) "\n"
                      "actual:\n" actual))
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
          #_ (pp/pprint actual)]
      (is (= actual expected)))))

(deftest alter-table-add-unique
  (testing "alter table add unique"
    (let [ents-before [{:name "test_table"
                        :fields [{:name "should-be-unique"
                                  :type :int}]}]
          ents-after [{:name "test_table"
                       :fields [{:name "should-be-unique"
                                 :type :int
                                 :unique true}]}]
          the-diff (dal/diffl (xform-ents ents-before) (xform-ents ents-after))
          #_ (println (str "the-diff:\n" (cljc-utils/pp the-diff)))
          actual (diff->ddl the-diff)
          #_ (println (str "actual:\n" (cljc-utils/pp actual)))
          expected (->> ["ALTER TABLE test_table"
                         "    ADD CONSTRAINT test_table_should_be_unique_unique UNIQUE (should_be_unique);"]
                        (interpose "\n")
                        (apply str))]
      (is (= actual expected)))))


(deftest alter-table-add-foreign-key
  (let [ents-before [{:name "test-table"
                      :fields [{:name "hi"
                                :type :int}]}
                     ]
        ents-after [{:name "new-something"
                     :fields [{:name "some-field"
                               :type :int}]}
                    {:name "test-table"
                     :fields [{:name "hi"
                               :type :int}
                              {:name "new-something"
                               :type :foreign-key
                               :references :new-something}]}]
        the-diff (dal/diffl (xform-ents ents-before) (xform-ents ents-after))
        actual (diff->ddl the-diff)
        expected (->> ["CREATE TABLE new_something ("
                       "    id SERIAL PRIMARY KEY,"
                       "    some_field INTEGER NOT NULL"
                       ");"
                       "ALTER TABLE test_table"
                       "    ADD COLUMN new_something_id INTEGER REFERENCES new_something NOT NULL;"]
                      (interpose "\n")
                      (apply str))
        #_ (println "\nexpected:")
        #_ (clojure.pprint/pprint expected)
        #_ (println "\nactual:")
        #_ (clojure.pprint/pprint actual)]
    (is (= actual expected))))


(deftest remove-add-not-null-1
  (let [ents-before [{:name "test-table"
                      :fields [{:name "a-field"
                                :type :character
                                :max-length 32
                                :null true}]}]
        ents-after [{:name "test-table"
                     :fields [{:name "a-field"
                               :type :character
                               :max-length 32}]}]
        the-diff (dal/diffl (xform-ents ents-before) (xform-ents ents-after))
        #_ (clojure.pprint/pprint the-diff)
        actual (diff->ddl the-diff)
        expected (->> ["ALTER TABLE test_table"
                       "    ALTER COLUMN a_field SET NOT NULL;"]
                      (interpose "\n")
                      (apply str))]
    (is (= actual expected))))

(deftest remove-add-not-null-2
  (let [ents-before [{:name "test-table"
                      :fields [{:name "a-field"
                                :type :character
                                :max-length 32
                                :null true}]}]
        ents-after [{:name "test-table"
                     :fields [{:name "a-field"
                               :type :character
                               :max-length 32
                               :null false}]}]
        the-diff (dal/diffl (xform-ents ents-before) (xform-ents ents-after))
        actual (diff->ddl the-diff)
        expected (->> ["ALTER TABLE test_table"
                       "    ALTER COLUMN a_field SET NOT NULL;"]
                      (interpose "\n")
                      (apply str))]
    (is (= actual expected))))

(deftest remove-drop-not-null-1
  (let [ents-before [{:name "test-table"
                      :fields [{:name "a-field"
                                :type :character
                                :max-length 32}]}]
        ents-after [{:name "test-table"
                     :fields [{:name "a-field"
                               :type :character
                               :max-length 32
                               :null true}]}]
        the-diff (dal/diffl (xform-ents ents-before) (xform-ents ents-after))
        actual (diff->ddl the-diff)
        expected (->> ["ALTER TABLE test_table"
                       "    ALTER COLUMN a_field DROP NOT NULL;"]
                      (interpose "\n")
                      (apply str))]
    (is (= expected actual))))

(deftest remove-drop-not-null-2
  (let [ents-before [{:name "test-table"
                      :fields [{:name "a-field"
                                :type :character
                                :max-length 32
                                :null false}]}]
        ents-after [{:name "test-table"
                     :fields [{:name "a-field"
                               :type :character
                               :max-length 32
                               :null true}]}]
        the-diff (dal/diffl (xform-ents ents-before) (xform-ents ents-after))
        #_ (clojure.pprint/pprint the-diff)
        actual (diff->ddl the-diff)
        expected (->> ["ALTER TABLE test_table"
                       "    ALTER COLUMN a_field DROP NOT NULL;"]
                      (interpose "\n")
                      (apply str))]
    (is (= expected actual))))
