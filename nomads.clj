(ns yamworld.nomads
  (use rielib.utils
       yamworld.comm)
  (import (java.io BufferedReader)))



;; Nomad - a user not yet logged in.
(def nomads (ref #{}))

(def nomad-states (ref {}))

(defn- nomad-set-state [nomad state]
  (dosync (commute nomad-states assoc nomad state)))


(defmacro nomad-state-case [nomad data-name & args]
  `(let [nomad-state-data# (@nomad-states ~nomad)
	 nomad-state# (if (coll? nomad-state-data#)
			(first nomad-state-data#)
			nomad-state-data#)
	 ~data-name (if (coll? nomad-state-data#)
		      (rest nomad-state-data#))
	 result# (if-let [new-state#
			(condp = nomad-state#
				 ~@(mapcat (fn [[key & forms]] `(~key (do ~@forms)))  args))]
		 new-state#
		 nomad-state#)]
     (dosync 
      (commute nomad-states assoc ~nomad result#))))


(def login-message "Hello and welcome to Yamworld!")


(defn correct-password? [name password]
  true)

(defn- listen-to-nomad [nomad]
  (doseq [line (line-seq (new BufferedReader (:ins nomad)))]
    (nomad-state-case nomad data
      (:login-prompt
       (write nomad "login:>")
       :login-name)
      (:login-name
       (write nomad "password:>")			; lookup account (if none exists, ask for password anyway.)
       [:password {:name line}])
      (:password
       (write nomad "You said your name was %s!"  data)
					; Check password (making sure account exists).
       (if (correct-password? (:name data) line)
	 :game-play ;note: when an actual function is avalible, this will upgrade the nomad to a player. (not change state)
	 :login-prompt))
      (:game-play
       (writeln nomad "*Game goes here*")
       (write nomad ":*game prompt goes here*>"))))
  ;game over man!
  )



(defn handle-nomad [conn]
  (dosync 
   (commute nomads conj conn))
  (writeln conn login-message)
  (write conn "login:>")
  (nomad-set-state conn :login-name)
  (listen-to-nomad conn))


