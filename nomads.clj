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


(ns yamworld.nomads
  (use rielib.utils
       rielib.seq
       [clojure.contrib.macro-utils :only [macrolet]])
  (require
   [clojure.contrib 
    [logging :as log]]
       [yamworld 
	[comm :as comm]
	[trash :as trash]])
  (import (java.io BufferedReader IOException)))


;; Nomad - a user not yet logged in.
(def nomads (ref #{}))
(defn- collect-menu-states [result [key hash]]
  (if (and hash
	   (hash :menu))
    (assoc result key (hash :menu))
    nil))


(defn- get-state [state]
  (if (vector? state)
    (first state)
    state))

;; (declare process-input-from-nomad)
;; ;; Ok, this is quite a mess, looks like I was trying to make a nice input macro
;; ;; but it all went terribly wrong. I think if I made it more functional it would
;; ;; be a lot cleaner and more vercital... vertical?
;; (defmacro state-dialog [nomad data-name input-name initial-state & args] ;; Broken!
;;   (let [arg-flags (reduce #(assoc %1 (first %2) (rehash-seq (pair-seq (second %2))))
;; 			  {} (filter #(vector? (second %)) args))
;; 	menu-states (reduce collect-menu-states {} arg-flags)]
;; ;;    (println arg-flags menu-states)
;;     `(let [nomad#   ~nomad
;; 	   buffer#  (new BufferedReader (:ins nomad#))
;; 	   menus#   ~menu-states]
;;        ;; If they set the menu flag, and its the default, instert the menu function.
;;        ~(if (and menu-states
;; 		 (menu-states initial-state))
;; 	  `(~(menu-states initial-state) ~nomad))
;;        ;; Main input loop
;;        (try 
;; 	(loop [~input-name (.readLine buffer#) state-var# ~initial-state]
;; 	  (let [state# (if (coll? state-var#)
;; 			 (first state-var#)
;; 			 state-var#) ; figure out the new state is
;; 		~data-name (if (coll? state-var#)
;; 			     (second state-var#)) ; collect any data
;; 		new-state#
;; 		     (condp = state# 
;; 			      ~@(mapcat (fn [[key & forms]] 
;; 					  `(~key (do
;; 						   ~@(if (vector? (first forms))
;; 						       (rest forms)
;; 						       forms)))) args))] ; remove any flag vectors and eval state.
;; 	    (cond 
;; 	     (and (coll? new-state#) 
;; 		  (= (first new-state#) :return)) ; if the new state has args, and is :return
;; 	     (second new-state#)                  ; return that data
;; 	     (= new-state# :return)               ; otherwise, if its just :return, return nil
;; 	     nil
;; 	     (nil? new-state#)                    ; if new-state is nil, leave state alown and read more input.
;; 	     (do (if (and menus#
;; 			  (menus# state#))
;; 		   ((menus# new-state#) nomad#))
;; 		 (recur (.readLine buffer#) state-var#))
;; 	     true                                 ; Otherwise, read more and change state.
;; 	     (do (if (and menus#
;; 			  (menus# (get-state new-state#)))
;; 		   ((menus# (get-state new-state#)) nomad#))
;; 	       (recur (.readLine buffer#) new-state#)))))

(defmulti nomad-preinput 
  "Takes 2 arguments, state and data. 
state - the current state of the nomad handler system, used as the dispatch
data - the data set by other post methods. 
return: a string to be printed to the user.
This is usualy used as a promped and or menu for the nomad state."
  (fn [state & _]
    state))

(defmulti nomad-postinput 
  "Takes 2 arguments, state and data. 
state - the current state of the nomad handler system, used as the dispatch
input - the line of input the user has typed in.
data - the data set by other post methods. 
  return: either a keyword, hash map, or nil.
  keyword, :close - closes the socket and says goodbye.
  hash map, a map containing the folowing keys
    :message, a message to be printed to the nomad
    :data, some data, usualy a hash map or nil, to set the global data to.
    :state, the new state to change to.
  nil, no action."
  (fn [state & _]
    state))



(defn nanny [nomad]
  "Handles all side effects for pre and post input functions in the nomad handler."
  (let [buffer  (new BufferedReader (:ins nomad))
	current-state (atom :welcome)
	current-data (atom nil)] 
    (comm/write (nomad :outs) (nomad-preinput :welcome nomad))
    (try 
     (loop [input (.readLine buffer)]
      (let [result (nomad-postinput @current-state input @current-data)]
	(cond 
	  (map? result)
	  (let [{:keys [state data message]} result]
	    (if state
	      (reset! current-state state))
	    (if data
	      (reset! current-data data))
	    (if message
	      (comm/write (nomad :outs) message))
	    (comm/write (nomad :outs) (nomad-preinput @current-state @current-data))
	    (recur (.readLine buffer)))
	  (keyword? result)
	  (condp = result
	    :close
	    (comm/disconnect nomad)
	    (do 
	      (log/error (format "Nomad %s in state %s returned a unknown keyword %s." 
				 nomad @current-state result))
	      (comm/write (nomad :outs) (nomad-preinput @current-state @current-data))
	      (recur (.readLine buffer))))
	  (nil? result)
	  (do (comm/write (nomad :outs) (nomad-preinput @current-state @current-data))
	      (recur (.readLine buffer)))
	  true
	  (do (log/error (format "Nomad %s got a strange result from postinput fn %s for state %s."
				nomad result @current-state))
	      (comm/write (nomad :outs) (nomad-preinput @current-state @current-data))
	      (recur (.readLine buffer))))))
     (catch IOException e#
       (if (= "Stream closed" (.getMessage e#)) ; If the stream is closed, then just return nil.
	 nil
	 (throw e#)))
     (catch RuntimeException e# ; (people love to wrap exceptions for some reason)
       (if-let [cause# (.getCause e#)]
	 (if (and
	      (instance? java.io.IOException cause#)
	      (= "Stream closed"  (.getMessage cause#)))
	   nil
	   (throw e#))
	 (throw e#))))))

	      
	      
	      



(defmacro def-nomad-inputs [mode pre post]
  `(do (defmethod nomad-preinput ~mode [~'state ~'data]
	 ~pre)
       (defmethod nomad-postinput ~mode [~'state ~'input ~'data]
	 ~post)))
  
(defn input-match [word input]
  (.startsWith word (.toLowerCase input)))

(def-nomad-inputs :welcome
  (str login-message
       "\n\r:>")
  (condp input-match input  
    "secret"
    {:state :secret :message "SECRET!!!!\n\r"}
    "quit"
    :close
    {:message (format "You said '%s'!\n\r" input)}))

(def-nomad-inputs :secret
  "Secret MODE!:>"
  {:state :welcome :message "Secret Message recieved!\n\r"})
      
     






;; (declare account-managment create-account)

;; (defn- listen-to-nomad [nomad]
;;   (let [lag (let [lagtime (atom 1)]
;; 	      (fn []
;; 		(Thread/sleep (* 400 @lagtime))
;; 		(swap! lagtime * 2)))] ;; Your basic failed password lagger. 
;;     (write nomad "login:>")
;;     (state-dialog nomad data input :login-name
;;       (:login-name 
;;        (if (not (valid-name? input))
;; 	 (do (println "Invalid name!")
;; 	     (write nomad "login:>")
;; 	     nil)
;; 	 (if (account-exists? input)
;; 	   (do (write nomad "password:>")			; lookup account (if none exists, ask for password anyway.)
;; 	       [:password {:name input}])
;; 	   (do (write nomad "No such account, would you like to make one? ")
;; 	       [:new-account {:name input}]))))
;;       (:password
;;        (if-let [pfile (login-player (:name data) input)]
;; 	 (do (account-managment nomad pfile)
;; 	     (writeln nomad "See you soon!")
;; 	     (disconnect nomad)
;; 	     nil)
;; 	 (do (writeln nomad "Incorrect password!")
;; 	     (lag)
;; 	     (write nomad "login:>")
;; 	     :login-name)))
;;        (:new-account 
;; 	(if (re-matches #"(?i-)y(es)?" input)
;; 	  (do (account-managment nomad (create-account nomad (:name data)))
;; 	      (writeln nomad "See you soon!")
;; 	      (disconnect nomad))
;; 	  (if (re-matches #"(?i-)n(o)?" input)
;; 	    (do
;; 	      (write nomad "login:>")
;; 	      :login-name)
;; 	    (do 
;; 	      (write nomad "yes or no. ")
;; 	      nil))))))
;;   ;game over man!
;;   )

;; (defmacro repeat-char 
;;   "Creates a string with char repeating times times. Creates the string at compile time if possible."
;;   [char times]
;;   (if (and (instance? Character char)
;; 	   (integer? times))
;;     (let [strbuilder (new StringBuilder)]
;;       (dotimes [x times]
;; 	(.append strbuilder char))
;;       (.toString strbuilder))
;;     `(let [strbuilder# (new StringBuilder)
;; 	   char# ~char]
;;       (dotimes [_# ~times]
;; 	(.append strbuilder# char#))
;;       (.toString strbuilder#))))



;; (defn show-account [nomad account]
;;   (let [banner (format "%s %s [points %s]" (:name account) (:email account) (:points account))]
;;     (writeln nomad banner)
;;     (writeln nomad (repeat-char \- (count banner)))
;;     (dotimes [num (count (:characters account))]
;;       (let [pc (nth (:characters account) num)]
;; 	(writeln nomad "%d- %s %s %s" num (:name pc) (:race pc) (:level pc))))))

;; (defn show-am-menu [nomad]
;;   (write nomad "c-Create Character")
;;   (writeln nomad "\td-Delete Character")
;;   (write nomad "e- Change Email")
;;   (writeln nomad "\tp- Change Password")
;;   (write nomad "w- Whos online")
;;   (writeln nomad "\t(n)- Play that character."))

  
;; (declare enter-game character-creation valid-email?)

;; (defn enter-game [nomad pfile]
;;   (writeln nomad "Under Construction!"))

;; (defn account-managment [nomad pfile]
;;   (let [pfile (atom pfile)]
;;     (state-dialog nomad data input :main-menu
;;       (:main-menu [:menu #(do (show-account % @pfile)
;; 			      (show-am-menu %))]
;;        (if-let [selection (re-matches #"[cdepq]|(?:[0-9]+)" input)]
;; 	 (if-let [num (parse-int selection)]
;; 	   (if (> (count (:characters @pfile)) num -1)
;; 	     (enter-game nomad (nth (:characters @pfile) num))
;; 	     (do (writeln nomad "No such character!")
;; 		 (write nomad ":>")
;; 		 nil))
;; 	   (condp =
;; 	    "c" 
;; 	    (enter-game nomad (save-player (swap! pfile add-character (character-creation nomad))))
;; 	    "d"
;; 	    (do
;; 	      (writeln nomad "Select a character to delete. (a - abort")
;; 	      (write nomad ":n>")
;; 	      :delete-character)
;; 	    "e"
;; 	    (do 
;; 	      write nomad ":email>"
;; 	      :change-mail)
;; 	    "p"
;; 	    (do 
;; 	      write nomad ":passwd>"
;; 	      :change-passwd)
;; 	    "w"
;; 	    (do
;; 	      ;; TODO make this say something useful.
;; 	      (writeln nomad "No one playes this game yet...")
;; 	      (write nomad ":>"))))
;; 	 (writeln nomad "Not an option!")))
;;       (:delete-character
;;        (if-let [selection (re-matches #"a|(?:[0-9]+)" input)]
;; 	 (if-let [num (parse-int selection)]
;; 	   (if (> (count (:characters @pfile)) num -1) ;; shouldn't be able to make a negitive, but just to be safe.
;; 	     (do 
;; 	       (write nomad "Delete %s? are you sure?" (:name (nth (:characters @pfile) num)))
;; 	       [:delete-character-confirm num])
;; 	     (do 
;; 	       (writeln nomad "No such character!")
;; 	       (write nomad ":n>")))
;; 	   (if (= "a" input)
;; 	     (do (writeln nomad "ABORTED!!!")
;; 		 :main-menu)
;; 	     (do (writeln nomad "You should probably get out more.")
;; 		 (write nomad ":n>"))))))
;;       (:delete-character-confirm
;;        (if-let [choice (re-matches #"(?i-)yes|no" input)]
;; 	 (cond
;; 	  (= "yes" input)
;; 	  (do (write nomad "Deleteing %s..." (:name (nth (:characters @pfile) num)))
;; 	      (trash (nth (:character @pfile) num))
;; 	      (save-player (swap! pfile remove-character num))
;; 	      (writeln nomad "Done")
;; 	      (writeln nomad "Note: A backup will be kept for a little while. You can contact an Immortal to see if recovery is possible. (Don't assume it isn't)")
;; 	      :main-menu)
;; 	  (= "no" input)
;; 	  (do (writeln nomad "Well, never mind then...")
;; 	      :main-menu))
;; 	 (if (= input "Why so serious?")
;; 	   (do (writeln nomad "Because you're about to delete one of your peoples!")
;; 	       (writeln nomad "NOW YES OR NO!")
;; 	       (write nomad ":>"))
;; 	   (do (writeln nomad "YES or NO!")
;; 	       (writeln nomad ":>")))))
;;       (:change-mail
;;        (if (valid-email? input)
;; 	 (do (save-player (swap! pfile change-email input))
;; 	     (writeln nomad "Email changed.")
;; 	     :main-menu)
;; 	 (do (writeln nomad "Invalid email!")
;; 	     (write nomad ":email>"))))
;;       (:change-passwd
;;        (write nomad ":conferm>")
;;        [:conferm-passwd input])
;;       (:conferm-passwd
;;        (if (= input data)
;; 	 (do (save-player (swap! pfile change-passwd input))
;; 	     (writeln nomad "Password changed.")
;; 	     :main-menu)
;; 	 (do (writeln nomad "Passwords do not match!")
;; 	     (write nomad ":passwd>")
;; 	     :change-passwd))))))



;; (defn valid-email? [email]
;;   (if-let [[full user host] (re-matches #"(\S+)@(\S+)" email)]
;;     (do (println full user host)
;; 	(try (java.net.InetAddress/getByName host)
;; 	     true
;; 	     (catch java.net.UnknownHostException e
;; 	       false)))
;;     false))
	

;; (defn create-account [nomad name]
;;   (write nomad ":passwd>")
;;   (state-dialog nomad data input :choose-passwd
;;     (:choose-passwd
;;      (write nomad ":confirm>")
;;      [:confirm-passwd input])
;;     (:confirm-passwd
     
;;      (if (= input  data)
;;        (do (println "Please provide an email.")
;; 	   (write nomad ":email>")
;; 	   [:get-email data])
;;        (do (println "Passwords didn't match")
;; 	   (write nomad ":passwd>")
;; 	   :choose-passwd)))
;;     (:get-email
;;      (if (valid-email? input)
;;        (do (println "Creating Account...")
;; 	   [:return (create-player name data input)])
;;        (do (println "Email is invalid!")
;; 	   (write nomad ":email>")
;; 	   nil)))))


(def login-message "Hello and welcome to Yamworld!")     

(defn handle-nomad [conn]
  (comm/writeln (:outs conn) login-message)
  (nanny conn))


