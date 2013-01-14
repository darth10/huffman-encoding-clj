(ns huffman-encoding-clj.trees
  (:use huffman-encoding-clj.core))

(defn pack-string
  "Partitions the characters in a string by their integer value."
  [str]
  (partition-by int (sort (seq str))))

(defn get-char-count
  "Maps a sequence of characters into a sequence of pairs.
   Each pair is a character and its occurence count."
  [chars]
  (map (fn [item] [(first item) (count item)])
       (sort-by count chars)))

(defn get-char-map
  "Returns a map of characters to their occurence count in a given string."
  [str]
  (into {} (get-char-count (pack-string str))))

(defn make-leaf-list
  "Converts a character-occurences map to a list of leaf nodes."
  [char-map]
  (sort-by #(:weight %) (map make-leaf char-map)))

(defn has-lesser-weight? [weight]
  #(< (:weight %) weight))

(defn reduce-tree-list
  "Converts a list of leaf nodes into a single fork node."
  [trees]
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

(defn make-code-tree
  "Creates a code tree for a given string."
  [str]
  (let [leaf-list (make-leaf-list (get-char-map str))]
    (reduce-tree-list leaf-list)))

(defn get-encode-map
  "Returns a map of characters to their encoded binary sequences for a code tree."
  [tree]
  (into {} (find-paths tree)))

(defn into-decode-map
  "Reverses an encode map into a decode map."
  [encode-map]
  (into {} (map vector (vals encode-map) (keys encode-map))))

(defn get-decode-map
  "Returns a map of sequences to their decoded characters for a code tree."
  [tree]
  (into-decode-map (get-encode-map tree)))
