(ns declarative-ddl.entities-var-alpha
  (:require [declarative-ddl.cljc.utils.core :as cljc-utils]
            [declarative-ddl.cljc.core :as cljc-core]))

(def entities (atom nil))

(defn set-entities! [ents-val]
  (reset! entities ents-val))

(def entities-as-map (atom nil))


(add-watch
 entities
 :watcher
 (fn [_key _atom old-val new-val]
   ;; (cljc-utils/log (str "new-val:\n" (cljc-utils/pp new-val)))
   ;; todo - FDH 4/12/19
   ;;   - move function xform-entities-for-diff to cljc
   ;;   - rename that function to somthing better, like convert-entities-to-map or something
   ;;   - do something like this thing below
   ;; (reset! entties-as-map (convert-entities-to-map new-val))
   (cljc-utils/log "entities updated")
   (let [ents-map (cljc-core/xform-entities-for-diff new-val)]
     ;; (cljc-utils/log (str "xformed:\n" (cljc-utils/pp ents-map)))
     (reset! entities-as-map ents-map))))
