(ns huffman-encoding-clj.core-test
  (:use clojure.test
        huffman-encoding-clj.core))

(def test-tree
  (make-fork
   (make-leaf [\a 10])
   (make-leaf [\b 5])))

(deftest has-char-test
  (testing "has-char? of a fork"
    (is (has-char? test-tree \a))
    (is (not (has-char? test-tree \c)))))

(deftest find-paths-test
  (testing "find-paths of a fork")
  (let [path-map (into {} (find-paths test-tree))]
    (is (contains? path-map \a))
    (is (= (get path-map \a) [1]))))
