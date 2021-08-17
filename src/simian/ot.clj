;; simian.ot
;; my silly attempt to implement operational transformation
;; which should eventually enable live editing.
;;
;; https://en.wikipedia.org/wiki/Operational_transformation

(ns simian.ot
    (:require [simian.utils :as utils])
    (:import [java.security MessageDigest]))

;; -------- cryptography: 

(defn sha256 
  "SHA-256 a la https://gist.github.com/kubek2k/8446062"
  [string]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes string "UTF-8"))]
    (apply str (map (partial format "%02x") digest))))

(def hash-record (memoize #(sha256 (str %))))

(def root-hash (hash-record ::root))

(defn verify-root [obj] (= (:previous obj) root-hash))

;; -------- records:

(defn record [action payload & {:keys [at from] 
              :or {from ::root}}]
  {:time (System/currentTimeMillis)
   :action (case action
             :insert ::insert
             :delete ::delete
             :default ::invalid)
   :payload payload
   :location at
   :previous (hash-record from)}) 

;; -------- document operations

(defmulti operate 
  "Apply a task to data. Don't call directly or 
   else no OT for you."

  (fn [task 
       location 
       payload
       data] task))

(defmethod operate ::insert
  [_ location payload data]
  (str (subs data 0 location) 
       payload 
       (subs data location (count data))))

(defmethod operate ::delete
  [_ location payload data]
  (str (subs data 0 location) 
       (subs data 
             (+ location payload) 
             (count data))))

;; -------- ledger operations

(defn generate-dummy-ledger 
  "Generate a dummy ledger. Used for testing and for fun."

  [n] 
  (into [] 
        (take (+ n 1) 
              (iterate 
                #(record :delete (rand-int 50) :at 2 :from %) 
                ::root))))

(defn verify-ledger
  "Verify the integrity of a ledger."

  [ledger]
  (and (= (-> ledger first str sha256) root-hash)
       (reduce = (utils/double-map 
                   #(= (hash-record %1) (:previous %2))
                   ledger))))

(defn record-fuzz
  "Generate a decimal from record hash to break sorts"

  [record]

  (if (= record ::root) 0
      (float (/ (reduce + 
                        (take 5 (map int (seq (hash-record record))))) 
                10000))))

(defn sort-records
  "Sort records by application order. 
   Note that returned object is not a valid ledger."

  [records]
  (sort-by #(+ (or (:time %) 0) (record-fuzz %)) records))



;; -------- record application


