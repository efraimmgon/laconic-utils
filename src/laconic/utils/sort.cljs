(ns laconic.utils.sort)

(defn without
  "Returns set s with x removed."
  [s x] (clojure.set/difference s #{x}))

(defn take-1
  "Returns the pair [element, s'] where s' is set s with element removed."
  [s] {:pre [(not (empty? s))]}
  (let [item (first s)]
    [item (without s item)]))

(defn no-incoming
  "Returns the set of nodes in graph g for which there are no incoming
  edges, where g is a map of nodes to sets of nodes."
  [g]
  (let [nodes (set (keys g))
        have-incoming (apply clojure.set/union (vals g))]
    (clojure.set/difference nodes have-incoming)))

(defn normalize
  "Returns g with empty outgoing edges added for nodes with incoming
  edges only.  Example: {:a #{:b}} => {:a #{:b}, :b #{}}"
  [g]
  (let [have-incoming (apply clojure.set/union (vals g))]
    (reduce #(if (get % %2) % (assoc % %2 #{})) g have-incoming)))

(defn topological-sort
  "Proposes a topological sort for directed graph g using Kahn's
   algorithm, where g is a map of nodes to sets of nodes. If g is
   cyclic, returns nil."
  ([g]
     (topological-sort (normalize g) [] (no-incoming g)))
  ([g l s]
     (if (empty? s)
       (when (every? empty? (vals g)) l)
       (let [[n s'] (take-1 s)
             m (g n)
             g' (reduce #(update-in % [n] without %2) g m)]
         (recur g' 
                (conj l n) 
                (clojure.set/union s' 
                                   (clojure.set/intersection 
                                     (no-incoming g') m)))))))

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

  (topological-sort {:k nil})
  (topological-sort acyclic-g) ;=> [3 5 7 8 10 11 2 9]
  (topological-sort cyclic-g)) ;=> nil
