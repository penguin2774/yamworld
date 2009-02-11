(ns yamworld.server
  (use clojure.contrib.server-socket
       rielib.utils
       yamworld.nomads
       yamworld.comm)
  (import (java.io InputStreamReader OutputStreamWriter)))


(defn client-connection [ins outs]
    (thread-safe 
      (binding [*out* (new OutputStreamWriter outs)]
	(handle-nomad (struct conn (new InputStreamReader ins) *out*)))))


(def server (create-server 4646 client-connection))


