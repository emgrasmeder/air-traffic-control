(ns air-traffic-control.events
  (:require [clojure.spec.alpha :as s]
            [clojure.instant :as instant]
            [clojure.string :as clj-str]))

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
