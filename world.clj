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
  (and (map? data) (isa? (:type data) :yamworld.types/thing)))

(def-clobber-fn thing [data]
  (thing? data)
  data
  (and (integer? data) (contains? @things data))
  (@things data))


(defn mobile? [data]
  "Tests data and returns true if data is a mobile."
  (and (map? data) (isa? (:type data) :yamworld.types/mobile)))

(def-clobber-fn mobile [data]
  (mobile? data)
  data
  (and (integer? data) (contains? @things data))
  (thing data))

(defn item? [data]
  "Tests data and returns true if data is a item."
  (and (map? data) (isa? (:type data) :yamworld.types/item)))

(def-clobber-fn item [data]
  (item? data)
  data
  (and (integer? data) (contains? @things data))
  (thing data))

(defn change-thing [vnum new-thing]
  (dosync
   (ref-set things (assoc vnum new-thing))))

(defn add-thing [vnum new-thing]
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
  (and (map? data) (isa? (:type data) :yamworld.types/room)))

(def-clobber-fn room [data]
  (room? data)
  data
  (integer? data)
  (@rooms data))
	

(defn change-room [vnum new-room]
  (dosync 
   (ref-set rooms (assoc vnum new-room))))

(defn add-room [vnum new-room]
  (change-room vnum new-room))

(defn delete-room
  ([vnum]
     (dosync
      (ref-set rooms (dissoc vnum))))
  ([vnum & vnums]
     (dosync
      (ref-set rooms (apply dissoc (cons vnum vnums))))))
