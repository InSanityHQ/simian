(ns simian.core
    (:require [simian.ot :as ot])
    (:gen-class))

(def test-root (ot/record :delete "cho" :at 489)) 

(ot/record :insert "tha" :at 12 
           :from test-root)

(ot/record-hash test-root)




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
