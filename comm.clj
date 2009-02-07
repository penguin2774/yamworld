(ns yamworld.comm
  (:use 
	rielib.utils
	yamworld.nomads)
  (:import (java.io InputStreamReader OutputStreamWriter)))


;; Conn[ection] - Any open sockets to the outside world.
(defstruct conn :ins :outs)


(defn write
  "Writes a string to stream, if more arguments are supplied, format is performed first."
  ([outs msg]
     (let [outs (if (map? outs) ; if its a map, assume its a conn otherwise assume its a outstream
		  (:outs outs)
		  outs)]
       (.write outs msg)
       (.flush outs)))
  ([outs msg & args]
     (write outs (apply format (cons msg args)))))

(defmacro writeln 
  "Same as write except appends an endline to the string."
  [outs msg & args]
  (concat `(write ~outs ~(if (instance? String msg) ; Appending an endline to string constents at runtime!
			   (.concat msg "\n\r") ; HA! Try and do that in C!
			   `(.concat ~msg "\n\r"))) args))



  