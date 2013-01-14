(ns huffman-encoding-clj.test-mocks
  (:use huffman-encoding-clj.core
        huffman-encoding-clj.trees)
  (:import java.util.UUID))

;; a random string
(def test-string (.toString (UUID/randomUUID)))

;; sample leaf list
(def test-leaf-list (make-leaf-list (get-char-map test-string)))

;; sample code tree
(def test-code-tree (make-code-tree test-string))
