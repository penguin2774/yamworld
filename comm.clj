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

(ns yamworld.comm
  (:use rielib.utils)
  (:import (java.io InputStreamReader OutputStreamWriter)))


;; Conn[ection] - Any open sockets to the outside world.
(defstruct conn :ins :outs)

(defn disconnect 
  "Disconnects the player."
  [conn]
  (.close (:ins conn)))

(defn write
  "Writes a string to stream, if more arguments are supplied, format is performed first."
  ([outs msg]
     (let [outs (if (map? outs) ; if its a map, assume its a conn otherwise assume its a outstream
		  (:outs outs)
		  outs)]
       (.write outs msg)
       (.flush outs))
     nil)
  ([outs msg & args]
     (write outs (apply format (cons msg args)))))

(defmacro writeln 
  "Same as write except appends an endline to the string."
  [outs msg & args]
  (concat `(write ~outs ~(if (instance? String msg) ; Appending an endline to string constents at runtime!
			   (.concat msg "\n\r") ; HA! Try and do that in C!
			   `(.concat ~msg "\n\r"))) args))



