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

(ns yamworld.world
  (:refer yamworld.types)
  (:use rielib.utils))


(def things (ref {}))

(defn vnum? [data]
  "Tests data and returns true if data is a vnum."
  (integer? data))

(def-clobber-fn vnum [data]
  (integer? data)
  data
  (and (map? data) (contains? data :vnum))
  (:vnum data))
  

(defn thing? [data]
  "Tests data and returns true if data is a thing."
  (and (map? data) (isa? (:type data) (type :thing))))

(def-clobber-fn thing [data]
  (thing? data)
  data
  (and (integer? data) (contains? @things data))
  (@things data))

(defn mobile? [data]
  "Tests data and returns true if data is a mobile."
  (and (map? data) (isa? (:type data) :mobile)))

(def-clobber-fn mobile [data]
  (mobile? data)
  data
  (and (integer? data) (contains? @things data))
  (thing data))

(defn item? [data]
  "Tests data and returns true if data is a item."
  (and (map? data) (isa? (:type data) :item)))

(def-clobber-fn item [data]
  (item? data)
  data
  (and (integer? data) (contains? @things data))
  (thing data))

(defn change-thing [vnum new-thing]
  (dosync
   (ref-set things (assoc vnum new-thing))))

(defn add-thing [vnum new-thing]
  (assert (thing? new-thing))
  (change-thing vnum new-thing)) ;; Shh! Don't tell!

(defn delete-thing 
  ([vnum]
     (dosync
      (ref-set things (dissoc vnum))))
  ([vnum & vnums]
     (dosync
      (ref-set things (apply dissoc (cons vnum vnums))))))

(def rooms (ref {}))

(defn room? [data]
  "Tests data and returns true if data is a room."
  (and (map? data) (isa? (:type data) (type :room))))

(def-clobber-fn room [data]
  (room? data)
  data
  (integer? data)
  (@rooms data))
	

(defn change-room [vnum new-room]
  (dosync 
   (ref-set rooms (assoc vnum new-room))))

(defn add-room [vnum new-room]
  (assert (room? new-room))
  (change-room vnum new-room))

(defn delete-room
  ([vnum]
     (dosync
      (ref-set rooms (dissoc vnum))))
  ([vnum & vnums]
     (dosync
      (ref-set rooms (apply dissoc (cons vnum vnums))))))



     


