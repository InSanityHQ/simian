;; simian.core
;; the monkey brain that powers simlink, but only barely

(ns simian.core
    (:require [simian.ot :as ot])
    (:gen-class))

(comment
  (def test-root (ot/record :delete "cho" :at 489)) 
  (ot/verify-root test-root)
  (ot/record :insert "tha" :at 12 :from test-root)
  (ot/record-hash test-root))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
