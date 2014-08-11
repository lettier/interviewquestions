;;;; David Lettier
;;;; (C) 2015.
;;;; http://www.lettier.com/

(ns red-black-tree.core
  (:gen-class))

(require '[clojure.core.match :refer [match]])

(defprotocol INode
  (get-data [this])
  (get-parent [this])
  (get-sibling [this])
  (get-grandparent [this])
  (get-uncle [this])
  (get-left [this])
  (get-right [this])
  (get-color [this])
  (set-data [this x])
  (set-parent [this x])
  (set-left [this x])
  (set-right [this x])
  (set-color [this x])
  (red? [this])
  (black? [this])
  (left-child? [this])
  (right-child? [this])
  (print-node [this]))

(deftype Node
         [^:volatile-mutable parent ^:volatile-mutable left ^:volatile-mutable right ^:volatile-mutable color ^:volatile-mutable data]
  INode
  (get-data [this] data)
  (get-parent [this] parent)
  (get-sibling [this]
    (let [parent (get-parent this)]
      (if-not (nil? parent)
        (if (left-child? this)
          (get-right parent)
          (get-left parent))
        nil)))
  (get-grandparent [this]
    (let [parent (get-parent this)]
      (if (nil? parent) nil (get-parent parent))))
  (get-uncle [this]
    (let [parent (get-parent this) grandparent (get-grandparent this)]
      (if (nil? grandparent)
        nil
        (if (left-child? parent)
          (get-right grandparent)
          (get-left grandparent)))))
  (get-left [this] left)
  (get-right [this] right)
  (get-color [this] color)
  (set-data [this x] (set! data x) this)
  (set-parent [this x] (set! parent x) this)
  (set-left [this x] (set! left x) this)
  (set-right [this x] (set! right x) this)
  (set-color [this x] (set! color x) this)
  (red? [this] (= (get-color this) "red"))
  (black? [this] (= (get-color this) "black"))
  (left-child? [this]
    (let [parent (get-parent this)]
      (if-not (nil? parent)
        (= (get-left parent) this)
        false)))
  (right-child? [this]
    (let [parent (get-parent this)]
      (if-not (nil? parent)
        (= (get-right parent) this)
        false)))
  (print-node [this]
    (println this
      "<#parent" (if-not (nil? (get-parent this)) (get-data (get-parent this)) nil) ">"
      "<#left" (if-not (nil? (get-left this)) (get-data (get-left this)) nil) ">"
      "<#right" (if-not (nil? (get-right this)) (get-data (get-right this)) nil) ">"
      "<#color" (get-color this) ">"
      "<#data" (get-data this) ">\n")
    this))

(extend-type nil
  INode
  (get-data [this] nil)
  (get-parent [this] nil)
  (get-sibling [this] nil)
  (get-grandparent [this] nil)
  (get-uncle [this] nil)
  (get-left [this] nil)
  (get-right [this] nil)
  (get-color [this] "black")
  (get-data [this] nil)
  (set-data [this x] nil)
  (set-parent [this x] nil)
  (set-left [this x] nil)
  (set-right [this x] nil)
  (set-color [this x] nil)
  (red? [this] false)
  (black? [this] true)
  (left-child? [this] false)
  (right-child? [this] false)
  (print-node [this] nil))

(defprotocol IRBTree
  (make-node [this p l r c d])
  (get-root [this])
  (set-root [this n])
  (insert [this x] [this n x])
  (insert1 [this n])
  (insert2 [this n])
  (insert3 [this n])
  (insert4 [this n])
  (insert5 [this n])
  (delete [this x] [this n x])
  (delete-fix [this n])
  (remove-sentinel [this s])
  (find-node [this x] [this n x])
  (min-node [this n])
  (rotate-left [this n])
  (rotate-right [this n])
  (clear-tree [this])
  (serialize-tree [this] [this n])
  (root? [this n])
  (print-tree [this] [this q]))

(deftype RBTree
         [^:volatile-mutable root]
  IRBTree
  (make-node [this p l r c d]
    (Node. p l r c d))
  (get-root [this] root)
  (set-root [this n] (set! root n) this)
  (insert [this x] (insert1 this (insert this nil x)) this)
  (insert [this n x]
    (if (nil? (get-root this))
      (if (nil? n)
        (let [node (make-node this nil nil nil "red" x)]
          (set-root this node)
          node)
        ((set-root this n) n))
      (if-not (nil? n)
        (let [data (get-data n)]
          (if-not (nil? data)
            (if (>= x data)
              (if-not (nil? (get-right n))
                (insert this (get-right n) x)
                (let [node (make-node this n nil nil "red" x)]
                  (set-right n node)
                  node))
              (if-not (nil? (get-left n))
                (insert this (get-left n) x)
                (let [node (make-node this n nil nil "red" x)]
                  (set-left n node)
                  node)))))
        (insert this (get-root this) x))))
  (insert1 [this n]
    (if (nil? (get-parent n))
      (set-color n "black")
      (insert2 this n)))
  (insert2 [this n]
    (if (= (get-color (get-parent n)) "black")
      n
      (insert3 this n)))
  (insert3 [this n]
    (let [uncle (get-uncle n) grandparent (get-grandparent n)]
      (if (and (not (nil? uncle)) (= (get-color uncle) "red"))
        (do
          (set-color (get-parent n) "black")
          (set-color uncle "black")
          (set-color grandparent "red")
          (insert1 this grandparent))
        (insert4 this n))))
  (insert4 [this n]
    (let [grandparent (get-grandparent n) parent (get-parent n)]
      (if (and (= (get-right parent) n) (= parent (get-left grandparent)))
        (do
          (rotate-left this parent)
          (insert5 this (get-left n)))
        (if (and (= (get-left parent) n) (= parent (get-right grandparent)))
          (do
            (rotate-right this parent)
            (insert5 this (get-right n)))
          (insert5 this n)))))
  (insert5 [this n]
    (if-not (nil? n)
      (let [grandparent (get-grandparent n) parent (get-parent n)]
        (do
          (set-color parent "black")
          (set-color grandparent "red")
          (if (= (get-left parent) n)
            (rotate-right this grandparent)
            (rotate-left this grandparent))))))
  (delete [this x]
    (let [result (delete this (get-root this) x)]
      (let [removed (first result) moved (second result)]
        (if-not (nil? (get-root this))
          (if (nil? (get-data (get-root this)))
            (set-root this nil)
            (if-not (nil? removed)
              (delete-fix this moved))))))
    this)
  (delete [this n x]
    (if-not (nil? n)
      (let [found (find-node this n x)]
        (if-not (nil? found)
          (match [(not (nil? (get-left found))) (not (nil? (get-right found)))]
            ; Has no children.
            [false false] (let [parent (get-parent found)]
              (if (nil? parent)
                (do (set-root this nil) [found nil])
                (let [sentinel (make-node this nil nil nil "black" nil)]
                  (if (left-child? found)
                    (set-left parent sentinel)
                    (set-right parent sentinel))
                  (set-parent sentinel parent)
                  (set-parent found nil)
                  [found sentinel])))
            ; Has only a left child.
            [true false] (let [parent (get-parent found) left (get-left found)]
              (if-not (nil? parent)
                (if (left-child? found)
                  (do
                    (set-left parent (get-left found))
                    (set-parent (get-left found) parent)
                    (set-left found nil) (set-parent found nil))
                  (do
                    (set-right parent (get-left found))
                    (set-parent (get-left found) parent)
                    (set-left found nil) (set-parent found nil)))
                (do (set-root this (get-left found)) (set-parent (get-left found) nil) (set-left found nil)))
              [found left])
            ; Has only a right child.
            [false true] (let [parent (get-parent found) right (get-right found)]
              (if-not (nil? parent)
                (if (left-child? found)
                  (do
                    (set-left parent (get-right found))
                    (set-parent (get-right found) parent)
                    (set-right found nil) (set-parent found nil))
                  (do
                    (set-right parent (get-right found))
                    (set-parent (get-right found) parent)
                    (set-right found nil) (set-parent found nil)))
                (do (set-root this (get-right found)) (set-parent (get-right found) nil) (set-right found nil)))
              [found right])
            ; Has two children.
            [true true] (let [node-min (min-node this (get-right found))]
              (set-data found (get-data node-min))
              (delete this (get-right found) (get-data found))))))))
  (delete-fix [this n]
    (if (and (not (root? this n)) (black? n))
      (let [parent (get-parent n)
              sibling (get-sibling n)
              sibling-left-child (get-left (get-sibling n))
              sibling-right-child (get-right (get-sibling n))]
        (if (left-child? n)
          (match [(red? sibling) (black? sibling-left-child) (black? sibling-right-child)]
            [true _ _] (do
              (set-color sibling "black")
              (set-color parent "red")
              (rotate-left this parent)
              (delete-fix this n))
            [false true true] (do
              (set-color sibling "red")
              (delete-fix this parent))
            [false false true] (do
              (set-color sibling-left-child "black")
              (set-color sibling "red")
              (rotate-right this sibling)
              (delete-fix this n))
            [false _ false] (do
              (set-color sibling (get-color parent))
              (set-color parent "black")
              (set-color sibling-right-child "black")
              (rotate-left this parent)
              (delete-fix this (get-root this))))
          (match [(red? sibling) (black? sibling-left-child) (black? sibling-right-child)]
            [true _ _] (do
              (set-color sibling "black")
              (set-color parent "red")
              (rotate-right this parent)
              (delete-fix this n))
            [false true true] (do
              (set-color sibling "red")
              (delete-fix this parent))
            [false true false] (do
              (set-color sibling-right-child "black")
              (set-color sibling "red")
              (rotate-left this sibling)
              (delete-fix this n))
            [false false _] (do
              (set-color sibling (get-color parent))
              (set-color parent "black")
              (set-color sibling-left-child "black")
              (rotate-right this parent)
              (delete-fix this (get-root this)))))))
    (set-color n "black")
    (if (nil? (get-data n))
      (remove-sentinel this n)))
  (remove-sentinel [this s]
    (if-not (nil? s)
      (if (nil? (get-data s))
        (let [parent (get-parent s)]
          (if (left-child? s)
            (set-left parent nil)
            (set-right parent nil))
          (set-parent s nil)))))
  (find-node [this x] (find-node this (get-root this) x))
  (find-node [this n x]
    (if-not (or (nil? n) (nil? x) (nil? (get-data n)))
      (match [(= (get-data n) x) (> (get-data n) x) (< (get-data n) x)]
        [true false false] n
        [false true false] (find-node this (get-left n) x)
        [false false true] (find-node this (get-right n) x)
        :else nil)
      nil))
  (min-node [this n]
    (if (nil? n)
      nil
      (if-not (nil? (get-left n))
        (min-node this (get-left n))
        n)))
  (rotate-left [this n]
    (let [parent (get-parent n) left (get-left n) right (get-right n)]
      (if-not (nil? right)
        (do
          (let [right-left (get-left right)]
            (set-parent right parent)
            (if-not (nil? parent)
              (if (= (get-left parent) n)
                (set-left parent right)
                (set-right parent right))
              (set-root this right))
            (set-left right n)
            (set-parent n right)
            (if-not (nil? right-left)
              (do
                (set-parent right-left n)
                (set-right n right-left))
              (set-right n nil)))))))
  (rotate-right [this n]
    (let [parent (get-parent n) left (get-left n) right (get-right n)]
      (if-not (nil? left)
        (do
          (let [left-right (get-right left)]
            (set-parent left parent)
            (if-not (nil? parent)
              (if (= (get-left parent) n)
                (set-left parent left)
                (set-right parent left))
              (set-root this left))
            (set-right left n)
            (set-parent n left)
            (if-not (nil? left-right)
              (do
                (set-parent left-right n)
                (set-left n left-right))
              (set-left n nil)))))))
  (clear-tree [this] (set-root this nil))
  (serialize-tree [this] (serialize-tree this (get-root this)))
  (serialize-tree [this n]
    (if-not (nil? n)
      (let [data []]
        (into data
          (into (vec (serialize-tree this (get-left n)))
            (into [(get-data n)]
              (into (vec (serialize-tree this (get-right n))) [])))))))
  (root? [this n]
    (= (get-root this) n))
  (print-tree [this] (println "<#RBTree\n") (print-tree this [(get-root this)]) (print ">\n") this)
  (print-tree [this q]
    (let [n (first q)]
      (if-not (nil? n)
        (do (print-node n)
          (print-tree this
            (vec (keep #(if-not (nil? %) %)
              (conj (vec (rest q)) (get-left n) (get-right n))))))))
    this))

(defn make-rbtree [x] (RBTree. x))

(defn -main
  "Implement a red black tree."
  [& args]
  (let [rbtree (make-rbtree nil)]
    (dorun (for [i (range 11)] (insert rbtree i)))
    (print-tree rbtree)
    (println (serialize-tree rbtree))
    (delete rbtree 6)
    (delete rbtree (get-data (get-root rbtree)))
    (print-tree rbtree)
    (println (serialize-tree rbtree))))
