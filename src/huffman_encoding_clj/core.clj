(ns huffman-encoding-clj.core)

;; Huffman coding is an entropy encoding algorithm
;; used for lossless data compression.

;; A given text or string requires the generation
;; of a binary code tree. The tree consists of leaf
;; nodes and internal fork nodes.

;; A lead node has a character and a weight, which
;; represents the probability of occurence of the
;; character in the given text.
(defrecord Leaf [char weight])

;; A fork represents an internal node with a left
;; and right subtree. Although a fork doesn't have
;; any characters, the chars field represents
;; the characters in the left and right subtrees
;; of the fork node.
(defrecord Fork [left right chars weight])

(defmethod print-method Leaf            ; prints a leaf node
  [leaf w]
  (print-simple (str "<" (:char leaf) "|" (:weight leaf) ">") w))

(defmethod print-method Fork            ; prints a fork node
  [fork w]
  (let [this (str " " (:weight fork) " ")]
    (print-simple "<" w)
    (print-method (:left fork) w)
    (print-simple this w)
    (print-method (:right fork) w)
    (print-simple ">" w)))

(defmulti get-chars
  "Gets the characters in a leaf or a fork node."
  class)

(defmethod get-chars Leaf
  [node]
  [(:char node)])

(defmethod get-chars Fork
  [node]
  (:chars node))

(defn has-char?
  "Checks if a character is present in a leaf or a fork node."
  [node c]
  (not (nil? (some #{c} (get-chars node)))))

(defn make-leaf
  "Creates a leaf node with character c and weight w."
  [[c w]]
  (->Leaf c w))

(defn make-fork
  "Creates a fork node from two nodes."
  [t1 t2]
  (let [weight1 (:weight t1)
        weight2 (:weight t2)
        w-sum (+ weight1 weight2)
        c-sum (concat (get-chars t1) (get-chars t2))
        [left right] (if (< weight1 weight2)
                       [t1 t2]
                       [t2 t1])]
    (->Fork left right c-sum w-sum)))

(defmulti find-paths
  "Finds all possible paths to leaf nodes in a tree."
  class)

(defn add-to-path
  "Adds a path field to a node. A path and a new path to add have to be provided"
  [tree path new-path]
  (find-paths
   (assoc tree :path (conj path new-path))))

(defmethod find-paths Leaf
  [node]
  {(:char node)
   (reverse (:path node))})

(defmethod find-paths Fork
  [node]
  (let [path (:path node)
        left-path (add-to-path (:left node) path 0)
        right-path (add-to-path (:right node) path 1)]
    (concat left-path right-path)))
