(ns air-traffic-control.events
  (:require [clojure.spec.alpha :as s]
            [clojure.instant :as instant]
            [clojure.string :as clj-str]
            [clojure.set :as clj-set]))

(s/def ::event 
  (s/or :update ::update 
        :removal ::removal))

(s/def ::update 
  (s/cat 
    :plane-id ::plane-id
    :model ::model
    :origin ::origin
    :destination ::destination
    :event-type ::event-type
    :timestamp ::timestamp
    :fuel-delta ::fuel-delta))
    
(s/def ::removal
  (s/cat 
    :plane-id ::plane-id
    :timestamp ::timestamp))

(defn int-able? 
  "Quick and dirty test to see if an input is an int or could be an int"
  [i?]
  (try (int? (Integer/parseInt (str i?)))
    (catch Exception _
      false)))

(s/def ::model int-able?)
(s/def ::origin string?)
(s/def ::destination string?)
(s/def ::event-type string?)
(s/def ::fuel-delta int-able?)


(s/def ::plane-id string?)
(s/def ::timestamp #(inst? (instant/read-instant-date %)))


(def new-event{:timestamp "2021-03-29T10:00:00", :event-type "Re-Fuel", :fuel-status "200", :status "OK"})


(defn add-event-to-flights-log 
  [new-event events]
  (-> new-event
      (select-keys [:timestamp :event-type :fuel-delta])
      (clj-set/rename-keys {:fuel-delta :fuel-status})
      (assoc :status "OK")
      (#(conj [] %))
      (concat events)
      (#(filter identity %))
      (#(sort-by :timestamp %))))

(defn insert
  "Inserts events into the program state. It should be an event sourcing solution but
  I don't have much experience with protocols and multimethods and figuring it out was going too slow
  so I opted for something that worked over."
  [state new-event]
  (swap! state #(update-in % [:state :flights (:plane-id new-event)] (partial add-event-to-flights-log new-event))))
