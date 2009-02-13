(ns yamworld.trash
  (:use rielib.utils)
  (:import (java.io File FileWriter)
	   (java.text DateFormat)))



(defn trash [data]
  (let [dir (new File ".dump/")]
    (if (not (.exists dir))
      (.mkdir dir)))
  (with-open [dump (new FileWriter (format ".dump/%s.garbage" 
					   (DateFormat/getDateInstance DateFormat/SHORT))
							       true)]
    (.write dump (prn-str data))))