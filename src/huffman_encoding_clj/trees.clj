(ns huffman-encoding-clj.trees
  (:use [huffman-encoding-clj.core]))

(defn pack-string [str]
  (partition-by int (sort (seq str))))

(defn get-char-count [chars]
  (map (fn [item] [(first item) (count item)])
       (sort-by count chars)))

(defn get-char-map [str]
  (into {} (get-char-count (pack-string str))))

(defn make-leaf-list [char-map]
  (map make-leaf char-map))

;; TODO not sure this really works. read theory and use tests from patmat
(defn make-code-tree [str]
  (let [leaf-list (make-leaf-list
                   (get-char-map str))]
    (reduce #(make-fork %1 %2)
            leaf-list)))
