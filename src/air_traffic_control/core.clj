(ns air-traffic-control.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
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


(defn time-comparator
  [t1 t2]
  (let [time1 (c/to-date-time t1)
        time2 (c/to-date-time t2)]
    (cond
      (t/equal? time1 time2)  0
      (t/before? time1 time2) -1
      (t/after? time1 time2)  1)))

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
