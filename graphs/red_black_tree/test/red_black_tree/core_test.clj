(ns red-black-tree.core-test
  (:require [clojure.test :refer :all]
            [red-black-tree.core :refer :all])
  (:use [clojure.math.numeric-tower :only [expt]]))

(deftest root-is-black
  (testing "tree root is black."
    (let [rbtree-test (make-rbtree nil)]
      (dorun (for [i (range 20)] (insert rbtree-test i)))
      (dorun (for [i (range 20 -1 -2)] (delete rbtree-test i)))
      (is (= "black" (get-color (get-root rbtree-test)))))))

(deftest black-or-red
  (testing "tree nodes are either black or red."
    (let [rbtree-test (make-rbtree nil)]
      (dorun (for [i (range 20)] (insert rbtree-test i)))
      (dorun (for [i (range 20 -1 -2)] (delete rbtree-test i)))
      (letfn [(bfs [t q]
                (let [node (first q)]
                  (if-not (nil? node)
                    (do
                      (is (or (= "black" (get-color node)) (= "red" (get-color node))))
                      (bfs t (vec (keep #(if-not (nil? %) %)
                                        (conj (vec (rest q)) (get-left node) (get-right node)))))))))]
        (bfs rbtree-test [(get-root rbtree-test)])))))

(deftest every-red-has-black-children
  (testing "if children exist, every red node's children are black."
    (let [rbtree-test (make-rbtree nil)]
      (dorun (for [i (range 20)] (insert rbtree-test i)))
      (dorun (for [i (range 20 -1 -2)] (delete rbtree-test i)))
      (letfn [(bfs [t q]
        (let [node (first q)]
          (if-not (nil? node)
            (do
              (if (= (get-color node) "red")
                (let [left (get-left node) right (get-right node)]
                  (if-not (nil? left)
                    (is (= (get-color left) "black")))
                  (if-not (nil? right)
                    (is (= (get-color right) "black")))))
              (bfs t (vec (keep #(if-not (nil? %) %)
                                (conj (vec (rest q)) (get-left node) (get-right node)))))))))]
        (bfs rbtree-test [(get-root rbtree-test)])))))

(deftest order-is-correct
  (testing "for every node, node->left is < node and node->right >= node."
    (let [rbtree-test (make-rbtree nil)]
      (dorun (for [i (range 20)] (insert rbtree-test i)))
      (dorun (for [i (range 20 -1 -2)] (delete rbtree-test i)))
      (defn bfs [t q]
        (let [node (first q)]
          (if-not (nil? node)
            (do
              (let [left (get-left node) right (get-right node)]
                (if-not (nil? left)
                  (is (< (get-data left) (get-data node))))
                (if-not (nil? right)
                  (is (>= (get-data right) (get-data node)))))
              (bfs t (vec (keep #(if-not (nil? %) %)
                                (conj (vec (rest q)) (get-left node) (get-right node)))))))))
      (bfs rbtree-test [(get-root rbtree-test)]))))

(deftest every-path-same-number-of-black
  (testing "for every path from the root to a leaf, they all have the same number of black nodes."
    (let [rbtree-test (make-rbtree nil) black-node-counts (atom ())]
      (dorun (for [i (range 32)] (insert rbtree-test i)))
      (dorun (for [i (range 32 -1 -2)] (delete rbtree-test i)))
      (print-tree rbtree-test)
      (defn traverse-tree [node black-node-count]
        (if-not (nil? node)
          (let [left (get-left node) right (get-right node) color (get-color node)]
            (if (= color "black")
              (do
                (if-not (nil? left)
                  (traverse-tree left (inc black-node-count)))
                (if-not (nil? right)
                  (traverse-tree right (inc black-node-count))))
              (do
                (if-not (nil? left)
                  (traverse-tree left black-node-count))
                (if-not (nil? right)
                  (traverse-tree right black-node-count))))
            (if (and (nil? left) (nil? right))
              (if (= color "black")
                (swap! black-node-counts conj (inc black-node-count))
                (swap! black-node-counts conj black-node-count))))))
      (traverse-tree (get-root rbtree-test) 0)
      (println @black-node-counts)
      (is (= (count (set @black-node-counts)) 1)))))

(deftest correct-nodes-were-deleted
  (testing "after deletion, the correct nodes are missing from the tree."
    (let [rbtree-test (make-rbtree nil)]
      (dorun (for [i (range 32)] (insert rbtree-test i)))
      (dorun (for [i (range 20 -1 -1)] (delete rbtree-test i)))
      (is (= (compare (serialize-tree rbtree-test) (vec (range 21 32))) 0))
      (dorun (for [i (range 21 32 2)] (delete rbtree-test i)))
      (is (= (compare (serialize-tree rbtree-test) (vec (range 22 32 2))) 0))
      (dorun (for [i (range 29)] (delete rbtree-test i)))
      (is (= (compare (serialize-tree rbtree-test) '[30]) 0))
      (delete rbtree-test 30)
      (is (= (compare (serialize-tree rbtree-test) nil) 0))
      (insert rbtree-test 5)
      (delete rbtree-test 5)
      (is (= (compare (serialize-tree rbtree-test) nil) 0))
      (insert rbtree-test 5)
      (insert rbtree-test 6)
      (delete rbtree-test 5)
      (is (= (compare (serialize-tree rbtree-test) '[6]) 0))
      (clear-tree rbtree-test)
      (insert rbtree-test 5)
      (insert rbtree-test 6)
      (delete rbtree-test 6)
      (is (= (compare (serialize-tree rbtree-test) '[5]) 0))
      (clear-tree rbtree-test)
      (insert rbtree-test 5)
      (insert rbtree-test 6)
      (insert rbtree-test 4)
      (delete rbtree-test 5)
      (is (= (compare (serialize-tree rbtree-test) '[4 6]) 0))
      (clear-tree rbtree-test)
      (insert rbtree-test 5)
      (insert rbtree-test 6)
      (insert rbtree-test 4)
      (insert rbtree-test 3)
      (insert rbtree-test 0)
      (insert rbtree-test 1)
      (delete rbtree-test 3)
      (delete rbtree-test 1)
      (is (= (compare (serialize-tree rbtree-test) '[0 4 5 6]) 0)))))

(deftest correct-height
  (testing "a perfect tree has the correct height"
    (def rbtree-test (make-rbtree nil))
    (def height 2)
    (insert rbtree-test 5)
    (insert rbtree-test 2)
    (insert rbtree-test 7)
    (insert rbtree-test 0)
    (insert rbtree-test 1)
    (insert rbtree-test 6)
    (insert rbtree-test 8)
    (defn tree-height [node]
      (if (nil? node)
        -1
        (max (tree-height (get-left node)) (+ (tree-height (get-right node)) 1))))
    (is (= (tree-height (get-root rbtree-test)) height))))
