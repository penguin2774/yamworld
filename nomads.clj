(ns yamworld.nomads
  (use rielib.utils
       yamworld.comm
       yamworld.players)
  (import (java.io BufferedReader IOException)))



;; Nomad - a user not yet logged in.
(def nomads (ref #{}))

;;(def nomad-states (ref {}))

;;(defn- nomad-set-state [nomad state]
;;  (dosync (commute nomad-states assoc nomad state)))


(defmacro state-dialog [nomad data-name input-name initial-state & args]
  `(let [nomad# ~nomad]
     (with-local-vars [state-var# ~initial-state]
       (try 
	(doseq [~input-name (line-seq (new BufferedReader (:ins nomad#)))]
	  (let [state# (if (coll? (var-get state-var#))
			 (first (var-get state-var#))
			 (var-get state-var#))
		~data-name (if (coll? (var-get state-var#))
			     (second (var-get state-var#)))]
	    (if-let [new-state#
		     (condp = state#
			     ~@(mapcat (fn [[key & forms]] `(~key (do ~@forms)))  args))]
	      (var-set state-var# new-state#))))
	(catch IOException e#
	  (if (= "Stream closed" (.getMessage e#))
	    nil
	    (throw e#)))
	(catch RuntimeException e#
	  (if-let [cause# (.getCause e#)]
	    (if (and
		 (instance? java.io.IOException cause#)
		 (= "Stream closed"  (.getMessage cause#)))
	      nil
	      (throw e#))
	    (throw e#)))))))
       


(def login-message "Hello and welcome to Yamworld!")



(declare account-managment create-account)

(defn- listen-to-nomad [nomad]
  (let [lag (let [lagtime (atom 1)]
	      (fn []
		(Thread/sleep (* 400 @lagtime))
		(swap! lagtime * 2)))] ;; Your basic failed password lagger. 
    (write nomad "login:>")
    (state-dialog nomad data input :login-name
      (:login-name
       (if (not (valid-name? input))
	 (do (println "Invalid name!")
	     (write nomad "login:>")
	     nil)
	 (if (account-exists? input)
	   (do (write nomad "password:>")			; lookup account (if none exists, ask for password anyway.)
	       [:password {:name input}])
	   (do (write nomad "No such account, would you like to make one? ")
	       [:new-account {:name input}]))))
      (:password
       (if-let [pfile (login-player (:name data) input)]
	 (do (account-managment nomad pfile)
	     (writeln nomad "See you soon!")
	     (disconnect nomad))
	 (do (writeln nomad "Incorrect password!")
	     (lag)
	     (write nomad "login:>")
	     :login-name)))
       (:new-account 
	(if (re-matches #"(?i-)y(es)?" input)
	  (do (account-managment nomad (create-account nomad (:name data)))
	      (writeln nomad "See you soon!")
	      (disconnect nomad))
	  (if (re-matches #"(?i-)n(o)?" input)
	    (do
	      (write nomad "login:>")
	      :login-name)
	    (do 
	      (write nomad "yes or no. ")
	      nil))))))
  ;game over man!
  )

(defn account-managment [nomad pfile]
  (println "Under Construction"))

(defn valid-email? [email]
  (if-let [[full user host] (re-matches #"(\S+)@(\S+)" email)]
    (do (println full user host)
	(try (java.net.InetAddress/getByName host)
	     true
	     (catch java.net.UnknownHostException e
	       false)))
    false))
	

(defn create-account [nomad name]
  (println "Also under construction"))

(defn handle-nomad [conn]
  (dosync 
   (commute nomads conj conn))
  (writeln conn login-message)
  (listen-to-nomad conn))


