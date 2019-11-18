(ns laconic.utils.sort)

;;; Depth First Search

; L ← Empty list that will contain the sorted nodes
; while there are unmarked nodes do
;     select an unmarked node n
;     visit(n)

; function visit(node n)
;     if n has a permanent mark then return
;     if n has a temporary mark then stop   (not a DAG)
;     mark n temporarily
;     for each node m with an edge from n to m do
;         visit(m)
;     mark n permanently
;     add n to head of L

(defn topological-sort 
  "Topological sort via depth first search. Takes a map of a node to
  a set of nodes. If there's a cycle returns nil."
  [nodes]
  (let [[lst temporary permanent] [(atom []) (atom #{}) (atom #{})]
        visit! (fn visit! [n]
                 (cond
                   (@permanent n) true
                   (@temporary n) (reduced nil) ; Not a DAG, stop.
                   :else
                   (do (swap! temporary conj n)
                       (if-not (reduce #(visit! %2) nil (nodes n))
                         (reduced nil)
                         (do
                           (swap! permanent conj n)
                           (swap! lst conj n))))))]
    (and
      (seq nodes)
      (reduce #(visit! %2) nil (keys nodes))
      @lst)))

(comment
  (def acyclic-g
    {7 #{11 8}
     5 #{11}
     3 #{8 10}
     11 #{2 9}
     8 #{9}})

  (def cyclic-g
    {7 #{11 8}
     5 #{11}
     3 #{8 10}
     11 #{2 9}
     8 #{9}
     2 #{11}}) ; oops, a cycle!

  (topological-sort acyclic-g) ;=> [3 5 7 8 10 11 2 9]
  (topological-sort cyclic-g)) ;=> nil
