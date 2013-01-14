(ns huffman-encoding-clj.trees-test
  (:use clojure.test
        huffman-encoding-clj.core
        huffman-encoding-clj.trees
        huffman-encoding-clj.test-mocks))

(deftest reduce-tree-list-test
  (testing "reduce-tree-list preserves weight of leaf nodes"
    (let [reduced (reduce-tree-list test-leaf-list)
          total-weight (reduce + (map #(:weight %) test-leaf-list))]
      (is (= (:weight reduced) total-weight)))))

(deftest code-tree-has-char-test
  (testing "code tree has all characters in string"
    (is (every? #(has-char? test-code-tree %) (seq test-string)))))

(deftest encode-map-test
  (testing "encode map has keys for all characters in string"
    (let [encode-map (get-encode-map test-code-tree)]
      (is (every? #(contains? encode-map %) (seq test-string))))))

(deftest decode-map-test
  (testing "decode map has values for all characters in string"
    (let [decode-map (get-decode-map test-code-tree)
          decode-values (vals decode-map)]
      (is (every? (set (seq test-string)) decode-values)))))
