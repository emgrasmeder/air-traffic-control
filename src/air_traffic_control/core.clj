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
  (events/insert immutable-state event-details))

(defmethod update-event-log
  :removal
  [[_ event-details]]
  (events/insert immutable-state event-details))

(defn parse-events
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
;
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


(defmulti calculate-fuel
  (fn [events]
    (->> events
         (map #(get % :event-type))
         (clj-str/join "-")
         (clj-str/lower-case)
         keyword)))


(defmethod calculate-fuel
  :land-take-off-re-fuel
  [events]
  (let [arrival-event (first (filter #(= "Land" (:event-type %)) events))
        refuel-event  (first (filter #(= "Re-Fuel" (:event-type %)) events))]
    (+ (Integer/parseInt (:fuel-status refuel-event))
       (Integer/parseInt (:fuel-status arrival-event)))))

(defmethod calculate-fuel
  :land
  [events]
  (let [arrival-event (first (filter #(= "Land" (:event-type %)) events))]
    (:fuel-status arrival-event)))

(defmethod calculate-fuel
  :re-fuel
  [events]
  (let [re-fuel-event (first (filter #(= "Re-Fuel" (:event-type %)) events))]
    (:fuel-status re-fuel-event)))

(defmethod calculate-fuel
  :take-off
  [_]
  0)

(defmethod calculate-fuel
  :default
  [params]
  (println "I don't do anything, so here's what I've been passed: " params)
  nil)


(defn calculate-fuel-diff
  "Could be very brittle as it is here. This assumes the input events will basically always have a Re-Fuel value or will have just departed"
  [events]
  (let [event       (first events)
        fuel-status (calculate-fuel events)]

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

(defn get-status-for-flight
  [time-query coll]
  (some-> coll
          (remove-timestamps-after time-query)
          remove-events-before-latest-refuel
          calculate-fuel-diff
          parse-filtered-flights))


(defn fetch-status-at
  "I'm not doing anything by way of error handling or considering the sad path
  But if I had more time I'd experiment with Failjure for handling some cases,
  like what if the requested time period is in the future
    
    I also don't chaining these map fns together. I often write recursive functions where
    for loops would usually be used. i don't know the right abstraction for this yet, though."
  [flights time-query]
  (let [time-query (c/to-date-time time-query)]
    (some->> flights
             :flights
             seq
             (map second)
             (map (partial get-status-for-flight time-query))
             (remove nil?)
             (clj-str/join "\n"))))
