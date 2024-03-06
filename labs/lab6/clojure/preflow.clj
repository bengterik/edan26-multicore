(require '[clojure.string :as str])		; for splitting an input line into words

(def debug false)

(def num-threads 2)

(defn prepend [list value] (cons value list))	; put value at the front of list

(defrecord node [i e h adj])			; index excess-preflow height adjacency-list

(defn node-adj [u] (:adj u))			; get the adjacency-list of a node
(defn node-height [u] (:h u))			; get the height of a node
(defn node-excess [u] (:e u))			; get the excess-preflow of a node

(defn has-excess [u nodes]
	(> (node-excess @(nodes u)) 0))

(defrecord edge [u v f c])			; one-node another-node flow capacity
(defn edge-flow [e] (:f e))			; get the current flow on an edge
(defn edge-capacity [e] (:c e))			; get the capacity of an edge

; read the m edges with the normal format "u v c"
(defn read-graph [i m nodes edges]
	(if (< i m)
		(do	(let [line 	(read-line)]			
			(let [words	(str/split line #" ") ]

			(let [u		(Integer/parseInt (first words))]
			(let [v 	(Integer/parseInt (first (rest words)))]
			(let [c 	(Integer/parseInt (first (rest (rest words))))]
			
			(ref-set (edges i) (update @(edges i) :u + u))
			(ref-set (edges i) (update @(edges i) :v + v))
			(ref-set (edges i) (update @(edges i) :c + c))

			(ref-set (nodes u) (update @(nodes u) :adj prepend i))
			(ref-set (nodes v) (update @(nodes v) :adj prepend i)))))))

			; read remaining edges
			(recur (+ i 1) m nodes edges))))

(defn other [edge u]
	(if (= (:u edge) u) (:v edge) (:u edge)))

(defn u-is-edge-u [edge u]
	(= (:u edge) u))

(defn increase-flow [edges i d]
	(ref-set (edges i) (update @(edges i) :f + d)))

(defn decrease-flow [edges i d]
	(ref-set (edges i) (update @(edges i) :f - d)))

(defn move-excess [nodes u v d]
	(ref-set (nodes u) (update @(nodes u) :e - d))
	(ref-set (nodes v) (update @(nodes v) :e + d)))

(defn insert [excess-nodes v]
	(ref-set excess-nodes (cons v @excess-nodes))
)

(defn check-insert [excess-nodes v s t]
	(if (and (not= v s) (not= v t))
		(insert excess-nodes v)))

(defn push [edge-index u nodes edges excess-nodes change s t]
	(let [v 	(other @(edges edge-index) u)]
	(let [uh	(node-height @(nodes u))]
	(let [vh	(node-height @(nodes v))]
	(let [e 	(node-excess @(nodes u))]
	(let [i		edge-index]
	(let [f 	(edge-flow @(edges i))]
	(let [c 	(edge-capacity @(edges i))]

	(if debug
		(do
			(println "--------- push -------------------")
			(println "i = " i)
			(println "u = " @(nodes u))
			(println "uh = " uh)
			(println "e = " e)
			(println "f = " f)
			(println "c = " c)
			(println "v = " @(nodes v))
			(println "vh = " vh)))
	
	(if (u-is-edge-u @(edges i) u)
		(do 
			(def d (min e (- c f)))
			(increase-flow edges i d))
		(do 
			(def d (min e (+ c f)))
			(decrease-flow edges i d)
			))

	(move-excess nodes u v d)
	(if (> e 0) (check-insert excess-nodes u s t))
	(if (= (node-excess @(nodes v)) d) (check-insert excess-nodes v s t))))))))))

(defn relabel [u nodes excess-nodes s t]
	(dosync
		(let [h	(node-height @(nodes u))]

		(ref-set (nodes u) (update @(nodes u) :h + 1))
		(check-insert excess-nodes u s t))))

; go through adjacency-list of source and push
(defn initial-push [adj s t nodes edges excess-nodes]
	(let [change (ref 0)] ; unused for initial pushes since we know they will be performed
	(if (not (empty? adj))
		(dosync
			; give source this capacity as excess so the push will be accepted
			(ref-set (nodes s) 
				(update @(nodes s) :e + 
					(edge-capacity @(edges (first adj)))))
			(push (first adj) s nodes edges excess-nodes change s t)
			(initial-push (rest adj) s t nodes edges excess-nodes)))))

(defn initial-pushes [nodes edges s t excess-nodes]
	(initial-push (node-adj @(nodes s)) s t nodes edges excess-nodes))

(defn remove-any [excess-nodes]
	(let [ u (ref -1)]
		(dosync 
			(if (not (empty? @excess-nodes))
				(do
					(ref-set u (first @excess-nodes))
					(ref-set excess-nodes (rest @excess-nodes))))
		@u)))

; read first line with n m c p from stdin

(def line (read-line))

; split it into words
(def words (str/split line #" "))

(def n (Integer/parseInt (first words)))
(def m (Integer/parseInt (first (rest words))))

(def s 0)
(def t (- n 1))
(def excess-nodes (ref ()))

(def nodes (vec (for [i (range n)] (ref (->node i 0 (if (= i 0) n 0) '())))))

(def edges (vec (for [i (range m)] (ref (->edge 0 0 0 0)))))

(dosync (read-graph 0 m nodes edges))

(defn work [nodes edges excess-nodes adj change u s t]	
	(let [e (first adj)]
	(if (not= e nil) 
		(do	
			(if (u-is-edge-u @(edges e) u)
				(do 
					(def v (:v @(edges e)))
					(def b 1))

				(do 
					(def v (:u @(edges e)))
					(def b -1)))
			

			(let [uh	(node-height @(nodes u))]
			(let [vh	(node-height @(nodes v))]
			(if (and (> uh vh) (< (* b (edge-flow @(edges e))) (edge-capacity @(edges e))))
				(do
					(dosync
					(push e u nodes edges excess-nodes change s t)
					(ref-set change (+ @change 1))))
				(do 	
					(work nodes edges excess-nodes (rest adj) change n s t)
				))))))))

(defn preflow []
	(dosync (initial-pushes nodes edges s t excess-nodes))
	(do 
		(while (not-empty @excess-nodes)
			(do
				(def n (remove-any excess-nodes))
				(let [change (ref 0)]
				(work nodes edges excess-nodes (:adj @(nodes n)) change n s t)
				(dosync
				(if (= @change 0) (relabel n nodes excess-nodes s t)))))))
	
	(println "f =" (node-excess @(nodes t))))

(defn main []
	(let [threads (repeatedly num-threads #(Thread. preflow))]
			(run! #(.start %) threads)
			(run! #(.join %) threads))
)
(preflow)
