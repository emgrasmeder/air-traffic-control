(ns air-traffic-control.core
  (:require [air-traffic-control.events :as events]
            [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]))

(defn parse-event [s]
  (let [maybe-event (clj-str/split s #" ")]
    (println (s/conform :events/timestamp (second maybe-event)))
    (s/conform :events/event maybe-event)))
    

(s/explain :events/removal (clj-str/split "F222 2020-03-03T10:00:00" #" "))




#_(defn parse-airplane-status 
    "Turns a single airplane status event into a map for easier use. 
  Probably a good use case for defrecord"
    [airplane-status]
    (zipmap [:plane-id :plane-model :origin :destination :event-type :timestamp :fuel-delta]
     (s/split airplane-status #" ")))

