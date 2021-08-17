;; simian.ot
;; my silly attempt to implement operational transformation
;; which should eventually enable live editing.
;;
;; https://en.wikipedia.org/wiki/Operational_transformation

(ns simian.ot
    (:import [java.security MessageDigest]))

;; -------- cryptography: 

(defn sha256 
  "SHA-256 a la https://gist.github.com/kubek2k/8446062"
  [string]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes string "UTF-8"))]
    (apply str (map (partial format "%02x") digest))))

(defn record-hash [record] (sha256 (str (:location record) 
                                        (:action record)
                                        (:time record))))

(def root-hash (memoize #(sha256 (str -1 ::root -1))))


(defn verify-root [obj] (= (root-hash) obj))

;; -------- edit objects:

(defn record [action payload & {:keys [at from] 
              :or {from {:time -1
              :action ::root
              :lbcation -1
              :previous -1}}}]
  {:time (System/currentTimeMillis)
   :action (case action
             :insert ::insert
             :delete ::delete
             :move ::move)
   :payload payload
   :location at
   :previous (hash-record from)}) 



