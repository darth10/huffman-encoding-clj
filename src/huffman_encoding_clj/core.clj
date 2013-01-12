(ns huffman-encoding-clj.core)

(defrecord Leaf [char weight])

(defrecord Fork [left right chars weight])

(defmethod print-method Leaf
  [leaf w]
  (print-simple (str "<" (:char leaf) "|" (:weight leaf) ">") w))

(defmethod print-method Fork
  [fork w]
  (let [this (str " "
                  (:weight fork)
                  " ")]
    (print-simple "<" w)
    (print-method (:left fork) w)
    (print-simple this w)
    (print-method (:right fork) w)
    (print-simple ">" w)))

(defmulti has-char?
  (fn [node c]  (class node)))

(defmethod has-char? Leaf
  [node c] (= (:char node) c))

(defmethod has-char? Fork
  [node c] (or (has-char? (:left node) c)
               (has-char? (:right node) c)))

(defmulti chars-sum class)

(defmethod chars-sum Leaf
  [node] [(:char node)])

(defmethod chars-sum Fork
  [node]
  (concat (chars-sum (:left node))
          (chars-sum (:right node))))

(defn make-leaf
  [[c w]] (->Leaf c w))

(defn make-fork [t1 t2]
  (let [weight1 (:weight t1)
        weight2 (:weight t2)
        w-sum (+ weight1 weight2)
        c-sum (concat (chars-sum t1) (chars-sum t2))
        [left right] (if (< weight1 weight2)
                       [t1 t2]
                       [t2 t1])]
    (->Fork left right c-sum w-sum)))
