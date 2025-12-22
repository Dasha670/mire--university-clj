(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *coins*)
(def ^:dynamic *level*)
(def ^:dynamic *experience*)

(def prompt "> ")
(def streams (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))

(defn get-coins []
  @*coins*)


(defn add-coins [amount]
  (dosync
   (alter *coins* + amount)))
