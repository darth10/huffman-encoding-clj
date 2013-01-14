(ns huffman-encoding-clj.encoding-test
  (:use clojure.test
        huffman-encoding-clj.encoding
        huffman-encoding-clj.test-mocks))

(deftest encode-error-test
  (testing "encode fails for unknown characters"
    (is (thrown? Error (encode-with-code-tree test-code-tree " ")))))

(deftest encode-tree-test
  (testing "encode-as-string doesn't need a code tree"
    (is (= (encode-as-string test-code-tree test-string)
           (encode-as-string test-string)))))

(deftest decode-test
  (testing "decoding an encoded string produces the original string"
    (is (= test-string (decode-seq-with-code-tree
                         test-code-tree
                         (encode-with-code-tree
                           test-code-tree
                           test-string))))))
