(ns huffman-encoding-clj.core)

(defrecord Leaf [char weight])

(defrecord Fork [left right chars weight])

(defmethod print-method Leaf
  [leaf w]
  (print-simple (str "<" (:char leaf) "|" (:weight leaf) ">") w))

(defmethod print-method Fork
  [fork w]
  (let [this (str " "
                  (apply str (seq (:chars fork))) "|"
                  (:weight fork)
                  " ")]
    (print-simple "<" w)
    (print-method (:left fork) w)
    (print-simple this w)
    (print-method (:right fork) w)
    (print-simple ">" w)))

(defmulti weight-sum class)

(defmethod weight-sum Leaf
  [node] (:weight node))

(defmethod weight-sum Fork
  [node] (+ (weight-sum (:left node))
            (weight-sum (:right node))))

(defmulti chars-sum class)

(defmethod chars-sum Leaf
  [node] [(:char node)])

(defmethod chars-sum Fork
  [node]
  (concat (chars-sum (:left node))
          (chars-sum (:right node))))

(defn make-leaf [c w]
  (Leaf. c w))

(defn make-fork [left right]
  (Fork. left right
         (concat (chars-sum left)
                 (chars-sum right))
         (+ (weight-sum left)
            (weight-sum right))))

;; TODO part 2 and 3
