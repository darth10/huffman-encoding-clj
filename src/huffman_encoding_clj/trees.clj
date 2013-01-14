(ns huffman-encoding-clj.trees
  (:use huffman-encoding-clj.core))

(defn pack-string [str]
  (partition-by int (sort (seq str))))

(defn get-char-count [chars]
  (map (fn [item] [(first item) (count item)])
       (sort-by count chars)))

(defn get-char-map [str]
  (into {} (get-char-count (pack-string str))))

(defn make-leaf-list [char-map]
  (sort-by #(:weight %) (map make-leaf char-map)))

(defn has-lesser-weight? [weight]
  #(< (:weight %) weight))

(defn reduce-tree-list [trees]
  (if (= 1 (count trees))
    (first trees)
    (let [[t1 t2] (take 2 trees)
          other-trees (drop 2 trees)
          new-tree (make-fork t1 t2)
          new-weight (:weight new-tree)
          [ts1 ts2] (partition-by (has-lesser-weight? new-weight)
                                  other-trees)
          next-trees (if (empty? ts2)
                       (cons new-tree ts1)
                       (concat ts1 [new-tree] ts2))]
      (recur next-trees))))

(defn make-code-tree [str]
  (let [leaf-list (make-leaf-list (get-char-map str))]
    (reduce-tree-list leaf-list)))

(defn get-encode-map [tree]
  (into {} (find-paths tree)))

(defn into-decode-map [encode-map]
  (into {} (map vector (vals encode-map) (keys encode-map))))

(defn get-decode-map [tree]
  (into-decode-map (get-encode-map tree)))
