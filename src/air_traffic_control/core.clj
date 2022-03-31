(ns air-traffic-control.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]
            [clj-time.coerce :as c]
            [clj-time.core :as clj-time]
            [air-traffic-control.events :as events]))

(defn parse-event
  [s]
  (let [maybe-event (clj-str/split s #" ")]
    (s/conform ::events/event maybe-event)))



(def initial-state {:state {:flights {}}})
(def immutable-state (atom initial-state))

(defmulti update-event-log first)
(defmethod update-event-log
  :update
  [[_ event-details]]
  (do
    (println "Handling update event")
    (events/insert immutable-state event-details)))

(defmethod update-event-log
  :removal
  [[_ event-details]]
  (do
    (println "Handling removal event")
    (events/invalidate immutable-state event-details)))

(defn parse-events!
  "This is the main entry point for inserting new data. "
  ([events]
   (some->> events
            (#(clj-str/split % #"\n"))
            (map parse-event)
            (run! update-event-log))))




(defn time-comparator
  [t1 t2]
  (let [time1 (c/to-date-time t1)
        time2 (c/to-date-time t2)]
    (cond
      (clj-time/equal? time1 time2) 0
      (clj-time/before? time1 time2) -1
      (clj-time/after? time1
                       time2)
      1)))

(defn sort-map-by
  "Written to compare timestamps, but could sort other maps using the `compare` comparator.
  I've never heard this word comparator but I think I got it from the docs for sort-map-by sooo..."
  ([m k]
   (sort-map-by m
                k
                (if (= k :timestamp)
                  time-comparator
                  compare)))
  ([m k _comparator]
   (into (sorted-map-by (fn [key1 key2]
                          (_comparator (get-in m [key1 k])
                                       (get-in m [key2 k]))))
         m)))

;probably should make remove-timestamps-after and remove-timestamps-before the same function
;already
(defn remove-timestamps-after
  "Relies on the fact that the events are insert in order such that the most recent timestamp is first, followed by the second most recent timestam, finally the timestamp in the most distant past is the last of the events"
  [events time-query]
  (->> events
       (remove #(clj-time/after? (c/to-date-time (:timestamp %))
                                 time-query))
       seq))


(defn remove-timestamps-before
  "Relies on the fact that the events are insert in order such that the most recent timestamp is first, followed by the second most recent timestam, finally the timestamp in the most distant past is the last of the events"
  [events time-query]
  (remove #(clj-time/before? (c/to-date-time (:timestamp %)) (c/to-date-time time-query))
          events))

(defn parse-filtered-flights
  "Basically the entire adapter layer here.
  We're taking some flight data that's using our ubiquitous language inside the program
  and converting it to readable format for our users. 
  Sometimes I put these functions in an 'adapters' namespace."
  [m]
  (some->> {"Re-Fuel"  "Awaiting-Takeoff"
            "Take-Off" "In-Flight"
            "Land"     "Landed"}
           seq
           (map (fn [[k v]]
                  (when (clojure.string/includes? (:event-type m) k)
                    (clojure.string/replace (:event-type m) k v))))
           (remove nil?)
           first
           (#(str (:plane-id m) " " % " " (:fuel-status m)))))

(defn event-dispatcher
  [events]
  (->> events
       (sort-by :timestamp)
       (map #(get % :event-type))
       (clj-str/join "-")
       (clj-str/lower-case)
       keyword))



(defmulti calculate-fuel
  event-dispatcher)


(defn find-fuel-value-for
  [events kw-filter]
  (->> events
       (filter #(= kw-filter (:event-type %)))
       first
       :fuel-status))


(defmethod calculate-fuel
  :land-take-off-re-fuel
  [events]
  (+ (Integer/parseInt (find-fuel-value-for events "Re-Fuel"))
     (Integer/parseInt (find-fuel-value-for events "Land"))))

(defmethod calculate-fuel
  :land
  [events]
  (Integer/parseInt (find-fuel-value-for events "Land")))

(defmethod calculate-fuel
  :re-fuel
  [events]
  (Integer/parseInt (find-fuel-value-for events "Re-Fuel")))

(defmethod calculate-fuel
  :take-off-re-fuel
  [events]
  (println "Missing 'Land' event, but accepting the new Re-Fuel event as legitimate")
  (Integer/parseInt (find-fuel-value-for events "Re-Fuel")))

(defmethod calculate-fuel
  :land-re-fuel
  [events]
  (find-fuel-value-for events "Re-Fuel"))

(defmethod calculate-fuel
  :land-take-off
  [events]
  (Integer/parseInt (find-fuel-value-for events "Land")))

(defmethod calculate-fuel
  :re-fuel-take-off
  [events]
  (Integer/parseInt (find-fuel-value-for events "Re-Fuel")))

(defmethod calculate-fuel
  :re-fuel-take-off-land
  [events]
  (+ (Integer/parseInt (find-fuel-value-for events "Re-Fuel"))
     (Integer/parseInt (find-fuel-value-for events "Land"))))


(defmethod calculate-fuel
  :re-fuel-land-take-off
  [events]
  (do
    (println "Found some wonky flight logs, but assuming the most recent event is most up to date")
    (Integer/parseInt (find-fuel-value-for events "Land"))))

(defmethod calculate-fuel
  :take-off-land
  [events]
  (find-fuel-value-for events "Land"))

(defmethod calculate-fuel
  :re-fuel-land
  [events]
  (find-fuel-value-for events "Land"))

(defmethod calculate-fuel
  :take-off
  [_]
  0)

(defmethod calculate-fuel
  :default
  [params]
  (do
    (println "I don't do anything, so here's what I've been passed: " params)
    (println "This was my dispatch value:" (event-dispatcher params)))
  nil)


(defn calculate-fuel-diff
  "Could be very brittle as it is here. This assumes the input events will basically always have a Re-Fuel value or will have just departed"
  [events]
  (let [event       (first events)
        fuel-status (calculate-fuel (filter #(= "OK" (:status %)) events))]

    {:plane-id    (:plane-id event)
     :event-type  (:event-type event)
     :fuel-status fuel-status}))


(defn find-latest-refuel-timestamp
  [events]
  (->> events
       (filter #(= "Re-Fuel" (:event-type %)))
       (sort-by :timestamp)
       first
       :timestamp))

(defn remove-events-before-latest-refuel
  "In all of the examples, the desired output value for fuel upon landing is equal to:
  fuel-value post refuel
  minus
  fuel-value post arrival
  so I assume the post-refuel value is the _amount of fuel in the tank_, and not an amount that has been _added_ to the tank.
  Thus we don't need to know about any events before the latest refueling"
  [events]
  (if (seq (filter #(= "Re-Fuel" (:event-type %)) events))
    (->> events
         find-latest-refuel-timestamp
         (remove-timestamps-before events)
         seq)
    events))

(defn filter-out-not-ok-events
  [events]
  (filter #(= "OK" (:status %)) events))


(defn get-status-for-flight
  [time-query coll]
  (some-> coll
          (remove-timestamps-after time-query)
          remove-events-before-latest-refuel
          filter-out-not-ok-events
          calculate-fuel-diff
          parse-filtered-flights))


(defn fetch-status-at
  "I'm not doing anything by way of error handling or considering the sad path
  But if I had more time I'd experiment with Failjure for handling some cases,
  like what if the requested time period is in the future
    
    I also don't chaining these map fns together. I often write recursive functions where
    for loops would usually be used. i don't know the right abstraction for this yet, though."
  ([time-query] (fetch-status-at (:state @immutable-state) time-query))
  ([flights time-query]
   (let [time-query (c/to-date-time time-query)]
     (some->> flights
              :flights
              seq
              (map second)
              (map (partial get-status-for-flight time-query))
              (remove nil?)
              (clj-str/join "\n")))))

(comment
  " You can play around with the code in either test/air-traffic-control.core-test 
  or right here. Adding an event causes a side effect in an atom, querying for that state
  assumes the atom has events in it. 
  
  `parse-events!` is your main function, which adds  events to the state
  `fetch-status-at` is the way to fetch data from that state map"


  (with-redefs [immutable-state (atom initial-state)]
    (let [input-events (str
                         "F111 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
                         "F2 747 start stop Take-Off 2000-01-01T00:00:00 0\n"
                         "F3 711 start stop Land 2000-01-01T00:00:00 100\n")
          _ (parse-events! input-events)
          result       (fetch-status-at "2022-01-02T00:00:00")]
      (println result))))

