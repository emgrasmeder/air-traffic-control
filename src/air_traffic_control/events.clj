(ns air-traffic-control.events
  (:require [clojure.spec.alpha :as s]
            [clojure.instant :as instant]
            [clojure.string :as clj-str]
            [clojure.set :as clj-set]))

(s/def ::event
  (s/or :update  ::update
        :removal ::removal))

(s/def ::update
  (s/cat
    :plane-id    ::plane-id
    :model       ::model
    :origin      ::origin
    :destination ::destination
    :event-type  ::event-type
    :timestamp   ::timestamp
    :fuel-delta  ::fuel-delta))

(s/def ::removal
  (s/cat
    :plane-id  ::plane-id
    :timestamp ::timestamp))

(defn int-able?
  "Quick and dirty test to see if an input is an int or could be an int"
  [i?]
  (try
    (int? (Integer/parseInt (str i?)))
    (catch Exception _
      false)))

(s/def ::model int-able?)
(s/def ::origin string?)
(s/def ::destination string?)
(s/def ::event-type string?)
(s/def ::fuel-delta int-able?)


(s/def ::plane-id string?)
(s/def ::timestamp #(inst? (instant/read-instant-date %)))



(defn update-maps-in-collection
  "updates maps whose have value v at key k in a collection"
  [collection k v updater]
  (reduce
    (fn [coll m]
      (conj coll
            (if (= (get m k) v)
              (updater m)
              m)))
    []
    collection))


(defn add-event-to-events
  "Sets status to INVALID if an event at the same timestamp already exists"
  ([new-event events] (add-event-to-events new-event events "OK"))
  ([new-event events status]
   (let [new-event-time (:timestamp new-event)
         new-event      (-> new-event
                            (assoc :status status)
                            (#(conj [] %)))]

     (-> events
         (update-maps-in-collection :timestamp
                                    new-event-time
                                    (fn [m]
                                      (assoc m :status "INVALID")))
         (concat new-event)))))



(defn invalidate-event-in-flights-log
  "Expects an event with :plane-id and :timestamp keys, finds any matching events
  and makes them as INVALID"
  [new-event events]
  (-> new-event
      (add-event-to-events events "INVALID")
      (#(filter identity %))
      (#(sort-by :timestamp %))
      reverse))


(defn add-event-to-flights-log
  [new-event events]
  (-> new-event
      (select-keys [:timestamp :plane-id :event-type :fuel-delta])
      (clj-set/rename-keys {:fuel-delta :fuel-status})
      (add-event-to-events events)
      (#(filter identity %))
      (#(sort-by :timestamp %))
      reverse))

(defn insert
  "Inserts events into the program state. It should be an event sourcing solution but
  I don't have much experience with protocols and multimethods and figuring it out was going too slow
  so I opted for this"
  [state new-event]
  (swap! state #(update-in %
                           [:state :flights (:plane-id new-event)]
                           (partial add-event-to-flights-log new-event))))

(defn invalidate
  "Inserts events into the program state. It should be an event sourcing solution but
  I don't have much experience with protocols and multimethods and figuring it out was going too slow
  so I opted for this"
  [state new-event]
  (swap! state #(update-in %
                           [:state :flights (:plane-id new-event)]
                           (partial invalidate-event-in-flights-log new-event))))
