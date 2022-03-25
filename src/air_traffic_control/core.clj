(ns air-traffic-control.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]
            [clj-time.coerce :as c]
            [clj-time.core :as t]))

(defn parse-event [s]
  (let [maybe-event (clj-str/split s #" ")]
    (s/conform :air-traffic-control.events/event maybe-event)))

(defmulti update-event-log first)

(defmethod update-event-log :update [[_ {:keys [plane-id model origin destination event-type timestamp fuel-delta]}]]
  (println "i'm parsing a thing as an update with"   plane-id model origin destination event-type timestamp fuel-delta))

(defmethod update-event-log :removal [[_ {:keys [plane-id timestamp]}]]
  (println "i'm parsing a thing as a removal with"   plane-id timestamp))

(comment
 (update-event-log (parse-event "F222 2021-03-29T10:00:00"))
 (update-event-log (parse-event "F324 313 LONDON NEWYORK Take-Off 2021-03-29T12:00:00 0")))




(defn time-comparator [t1 t2]
  (let [time1 (c/to-date-time t1)
        time2  (c/to-date-time t2)]
   (cond
    (t/equal? time1 time2) 0
    (t/before? time1 time2) -1
    (t/after? time1 time2) 1)))

(defn sort-map-by  
  "Written to compare timestamps, but could in theory sort other maps using the `compare` comparator"
  ([m k] (sort-map-by m k (if (= k :timestamp) time-comparator compare)))
  ([m k _comparator]
   (into (sorted-map-by (fn [key1 key2]
                         (_comparator
                              (get-in m [key1 k])
                              (get-in m [key2 k]))))
       m)))
