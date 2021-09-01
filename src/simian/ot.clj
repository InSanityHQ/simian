;; simian.ot
;; my silly attempt to implement operational transformation
;; which should eventually enable live editing.
;;
;; https://en.wikipedia.org/wiki/Operational_transformation

(ns simian.ot
    (:require [simian.utils :as utils])
    (:import [java.security MessageDigest]))

;; -------- cryptography

(defn sha256 
  "SHA-256 a la https://gist.github.com/kubek2k/8446062"
  [string]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes string "UTF-8"))]
    (apply str (map (partial format "%02x") digest))))

(def hash-record (memoize #(sha256 (str %))))

(def root-hash (hash-record ::root))

(defn verify-root [obj] (= (:previous obj) root-hash))

;; -------- records

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

;; -------- actual operational transformation!
;; Pran:
;; There maybe a central server, it seems like no one has
;; been able to do it without central server.
;;
;; # Documents based off of different states
;; Don't apply the actual edits for laggent buffer until
;; the edits that would render similar states have been
;; downloaded and applied.
;;
;; # User Story
;; Goal: Alice wants to write "cheeze".
;;
;; 1. Her local buffer becomes populated with the word cheeze.
;; 2. She broadcasts the request to write "cheeze" with the group.
;; 3. Her previous hash is too old to Bob and hence Bob rejects her update.
;;    She proceeds to ask to be submitted with Susan, who rejects her
;;    update because of its age as well. She proceeds to ask to be submitted
;;    with Jaqueline, who accepts her update as she is based on the same state
;;    as her current state.
;;    Meanwhile... Jaqueline tries to submit her pending chain of Alice => Her
;;    updates and becomes dejected as she is rejected by Bob, Susan for being
;;    based on an old state.
;;    Though she did find luck being accepted by Alice.
;; 4. Alice recieves new changes from Bob that updates her chain based on an older
;;    timeframe than her current state. She applies those changes cronologcially
;;    and transforms her and Jaqueline's update to fit that of Bob's.
;;    (We'd assume the same would be happening to Jaqueline as we speak,
;;    and transforming and reapplying a la Bob's Updates => Alice' incorrectly
;;    applied update => Jaqueline's own update.)
;; 5. She submits a new update with that operation to Bob, Susan, and
;;    Jaqueline. Bob and Susan both share a base state with her update,
;;    and has no future edits (Alice has newest state). So her edit is cleanly
;;    accepted.
;; 6. Jaqueline's subsequent update with the same package becomes accepted by
;;    all but immediately gets thrown out as it was already applied.
;;
;; # Dumb Edge Design Features/Cases
;; 1. "Speak until they listen": if a peer thinks your update pack's root is
;;    too old, wait until more updates come in and try again with that peer.
;;    If a peer think your update pack's root is too new, try again with an
;;    older state, and an older state, and an older state, etc... Until you
;;    find the last shared state. In this manner, the peer you are talking
;;    to will get the accepted updates too.
;; 2. Need to know states. Each edit from "Root/0" should have an indexed int
;;    that tracks the ledger number. It should be able to be calculated from
;;    three values: the value from the previous root, the content of the update
;;    and the ID. This value need to be overloaded with the subtraction operator
;;    and the int comparison operator.
;; 3. What if two operations are based on the same code? The ealier one gets
;;    applied first, then the second one operatinally transformed and assigned
;;    new ID += 1, then applied.
;; 4. What if we have {:alice {:op 3 :edit "INS a 12" :time 12 :prev 2}
;;                     :bob   {:op 3 :edit "DEL a 12" :time 12 :prev 2}
;;    The one with the smallest record-fuzz (any 'ol nonrandom, static algorithum
;;    like the one proposed here would work that creates a decimal as a function
;;    of previous state, time, and edit) would be the tie-breaker.
;;
;; ... let's hope this works

;; for the word "cheeze 


;; (defn generate-ticket
;;   "Generate action tickets based on a bunch
;;    of correctly-sorted records based on the same root."

;;   [records]
;;   (assert (apply = (map :previous records)) 
;;           "Records are not based on the same root!")
;;   (let [processing (first records)
;;         queue (rest records)
;;         cursor (:location processing)
;;         influence (if (= (:action processing) ::delete) 
;;                       (* -1 (record-influence processing)) 
;;                       (record-influence processing))]
;;     (map #(if (>= (:location %) cursor)
;;               (assoc % :location (+ (:location %) influence))
;;               %) queue)))

(record :insert "haahaa" :at 12)

;(record-fuzz (-> dummy-ledger rest rest first))

;(sort-records  dummy-ledger) 
