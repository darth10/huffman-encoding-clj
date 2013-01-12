(ns huffman-encoding-clj.encoding
  (:use huffman-encoding-clj.core
        huffman-encoding-clj.trees))

(defn encode-with-code-tree [tree s]
  (if (not (every? #(has-char? tree %) (seq s)))
    (throw (Error. "Unknown characters in string")))
  (let [chars (seq s)
        encode-map (get-encode-map tree)]
    (mapcat #(get encode-map %) chars)))

(defn encode-as-string
  ([s] (encode-as-string (make-code-tree s) s))
  ([tree s] (apply str (map str (encode-with-code-tree tree s)))))

(defn decode-seq-with-decode-map [decode-map s]
  (loop [to-decode s accu [] result []]
    (let [[first-to-decode & rest-to-decode] to-decode]
      (cond
       (contains? decode-map accu) (recur rest-to-decode
                                          (vector first-to-decode)
                                          (conj result (get decode-map accu)))
       (empty? to-decode) (apply str result)
       :else (recur rest-to-decode
                    (conj accu first-to-decode)
                    result)))))

(defn decode-seq-with-code-tree [tree s]
  (decode-seq-with-decode-map (get-decode-map tree) s))

(defn decode-string-with-code-tree [tree s]
  (decode-seq-with-code-tree tree
    (map #(Integer/parseInt (str %)) (seq s))))
