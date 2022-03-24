(ns air-traffic-control.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]))

(defn parse-event [s]
  (let [maybe-event (clj-str/split s #" ")]
    (s/conform :air-traffic-control.events/event maybe-event)))

(defmulti update-event-log first)

(defmethod update-event-log :update [[_ {:keys [plane-id model origin destination event-type timestamp fuel-delta]}]]
  (println "i'm parsing a thing as an update with"   plane-id model origin destination event-type timestamp fuel-delta))

(defmethod update-event-log :removal [[_ {:keys [plane-id timestamp]}]]
  (println "i'm parsing a thing as a removal with"   plane-id timestamp))

(update-event-log (parse-event "F222 2021-03-29T10:00:00"))
(update-event-log (parse-event "F324 313 LONDON NEWYORK Take-Off 2021-03-29T12:00:00 0"))
    
(s/explain :events/removal (clj-str/split "F222 2020-03-03T10:00:00" #" "))
