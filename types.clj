(ns yamworld.types
  (:use rielib.utils))

(def types-tree 
     [::type
      [::thing 
       [::mobile 
	[::character
	 ::pc
	 ::npc]]
       ::item]
      ::room])

(defn derive-type-tree []
	    (let [listing (tree-seq coll? seq types-tree)]
	      (doseq [[parent & children] (take-when coll? listing)]
		(doseq [child children]
		  (derive (if (coll? child)
			    (first child)
			    child) parent)))
	      (zipmap (map #(keyword (name %)) (take-when keyword? listing))
		      (take-when keyword? listing))))

(def types (derive-type-tree))

(defmacro type [key]
  (if (keyword? key)
    (types key)
    `(types ~key)))


  
  
  
  
		   

	


