;; simian.utils
;; utility functions for sheer entertainment and utility

(ns simian.utils)

(defn double-map
  "utility function to map pairwise items"
  
  [f coll]

  (map f coll (rest coll)))

