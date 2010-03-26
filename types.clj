;; The MIT License

;; Copyright (c) 2010 Nathanael Cunningham

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

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
	      (doseq [[parent & children] (filter coll? listing)]
		(doseq [child children]
		  (derive (if (coll? child)
			    (first child)
			    child) parent)))
	      (zipmap (map #(keyword (name %)) (filter keyword? listing))
		      (filter keyword? listing))))

(def types (derive-type-tree))

(defmacro type [key]
  (if (keyword? key)
    (types key)
    `(types ~key)))


  
  
  
  
		   

	


