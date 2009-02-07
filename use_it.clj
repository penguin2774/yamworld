(ns yamworld.use-it
  (use rielib.utils))


(defmulti use-it
  "Item is used by target."
  (fn [item target & other-args]
		(:type item)))