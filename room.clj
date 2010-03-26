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

(ns yamworld.room
  (use rielib.utils
       yamworld.use-it
       yamworld.types
       yamworld.world))


;; Note: room storage is handled by yamworld.world.

(defstruct room :title :desc :exits :contents)
; title - string
; desc - string
; exits - map (keyed to directions)
; contents - set


; Term exit: Refers to a direction vnum/exit-info pair 

(def direction?)

(defn exit? [data]
  (and (coll? data) (direction? (first data)) (vnum? (second data))))

(def-clobber-fn exit [data]
  (exit? data)
  data
  (and (map? data) (contains? data :dir) (contains? data :vnum))
  [(:dir data) (:vnum data)])

; Term direction: refers to an exit to a room, refers to a keyword but may be stored with other exit-info.

(defn direction? [data]
  (keyword? data))


(def-clobber-fn direction [data]
  (direction? data)
  data
  (string? data)
  (keyword data)
  (and (map? data) (not (coll? (:dir data))))
  (direction (:dir data))
  (and (coll? data) (not (coll? (first data))))
  (direction (first data)))

(defn take-exit [thing from dir]
  (let [dir (direction dir)]
    (dosync ; Make the world hold still for a moment.
     (if-let [to (room ((:exits from) dir))]
       (do 
	 (change-room (vnum from)
		      (assoc from :contents (disj (:contents from) thing)))
	 (change-room (vnum to)
		      (assoc to :contents (conj (:contents to) thing))))
       (error "Exit %s in room %s leads nowhere?" dir from)))))

(defmulti leave-room 
  "Called when something leaves a room."
  (fn [room thing & args]
    (:type room)))

;; probably should tell the room.
;;(defmethod leave-room (type :room) [room thing & args])
  
  

(defmulti enter-room
  "Called when something enters a room"
  (fn [room thing & args]
    (:type room)))
  
      
