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
  `(let [nomad# ~nomad
	  buffer#  (new BufferedReader (:ins nomad#))]
       (try 
	(loop [~input-name (.readLine buffer#) state-var# ~initial-state]
	  (let [state# (if (coll? state-var#)
			 (first state-var#)
			 state-var#)
		~data-name (if (coll? state-var#)
			     (second state-var#))
		new-state#
		     (condp = state#
			      ~@(mapcat (fn [[key & forms]] `(~key (do ~@forms)))  args))]
	      (cond 
	       (and (coll? new-state#) 
		    (= (first new-state#) :return))
	       (second new-state#)
	       (= new-state# :return)
	       nil
	       (nil? new-state#)
	       (recur (.readLine buffer#) state-var#)
	       true
	       (recur (.readLine buffer#) new-state#))))
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
	      (throw e#))))))
       


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
  (write nomad ":passwd>")
  (state-dialog nomad data input :choose-passwd
    (:choose-passwd
     (write nomad ":confirm>")
     [:confirm-passwd {:passwd input}])
    (:confirm-passwd
     
     (if (= input (:passwd data))
       (do (println "Please provide an email.")
	   (write nomad ":email>")
	   [:get-email data])
       (do (println "Passwords didn't match")
	   (write nomad ":passwd>")
	   :choose-passwd)))
    (:get-email
     (if (valid-email? input)
       (do (println "Creating Account...")
	   [:return (create-player name (data :password) input)])
       (do (println "Email is invalid!")
	   (write nomad ":email>")
	   nil)))))
       
     

     

(defn handle-nomad [conn]
  (dosync 
   (commute nomads conj conn))
  (writeln conn login-message)
  (listen-to-nomad conn))


