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
  
      
