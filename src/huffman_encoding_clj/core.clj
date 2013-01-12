(ns huffman-encoding-clj.core)

(defrecord Leaf [char weight])

(defrecord Fork [left right chars weight])

(defmethod print-method Leaf
  [leaf w]
  (print-simple (str "<" (:char leaf) "|" (:weight leaf) ">") w))

(defmethod print-method Fork
  [fork w]
  (let [this (str " " (:weight fork) " ")]
    (print-simple "<" w)
    (print-method (:left fork) w)
    (print-simple this w)
    (print-method (:right fork) w)
    (print-simple ">" w)))

(defmulti get-chars class)

(defmethod get-chars Leaf
  [node]
  [(:char node)])

(defmethod get-chars Fork
  [node]
  (:chars node))

(defn has-char? [node c]
  (not (nil? (some #{c} (get-chars node)))))

(defn make-leaf
  [[c w]] (->Leaf c w))

(defn make-fork [t1 t2]
  (let [weight1 (:weight t1)
        weight2 (:weight t2)
        w-sum (+ weight1 weight2)
        c-sum (concat (get-chars t1) (get-chars t2))
        [left right] (if (< weight1 weight2)
                       [t1 t2]
                       [t2 t1])]
    (->Fork left right c-sum w-sum)))

(defmulti find-paths class)

(defn add-to-path [tree path new-path]
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
