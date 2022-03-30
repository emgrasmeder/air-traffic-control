(ns air-traffic-control.events-test
  (:require [clojure.test :as t]
            [air-traffic-control.events :as events]
            [air-traffic-control.core :as atc]
            [clj-time.core :as clj-time]
            [clj-time.local :as l]
            [clj-time.coerce :as c]
            [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]))

(def now (clj-time/now))
(def yesterday (clj-time/minus now (clj-time/days 1)))
(def tomorrow (clj-time/plus now (clj-time/days 1)))
(def last-week (clj-time/minus now (clj-time/days 8)))
(def last-month (clj-time/minus now (clj-time/days 40)))

(t/deftest add-event-to-flights-log-test
  (t/testing "should not invalidate entries that dont have the same timestamp"
    (let [events            []
          yesterday-event   {:plane-id    "F222"
                             :model       "747"
                             :origin      "DUBLIN"
                             :destination "LONDON"
                             :event-type  "Re-Fuel"
                             :timestamp   yesterday
                             :fuel-delta  "200"}
          yesterday-results (events/add-event-to-flights-log yesterday-event events)
          last-week-event   {:plane-id    "F222"
                             :model       "747"
                             :origin      "DUBLIN"
                             :destination "LONDON"
                             :event-type  "Re-Fuel"
                             :timestamp   last-week
                             :fuel-delta  "200"}
          final-results     (events/add-event-to-flights-log last-week-event yesterday-results)
          valid-results     (filter #(= "OK" (:status %)) final-results)
          invalid-results   (filter #(= "INVALID" (:status %)) final-results)]
      (t/is (= 2 (count valid-results)))))

  (t/testing "should invalidate entries that have the same timestamp"
    (let [events            []
          yesterday-event   {:plane-id    "F222"
                             :model       "747"
                             :origin      "DUBLIN"
                             :destination "LONDON"
                             :event-type  "Re-Fuel"
                             :timestamp   yesterday
                             :fuel-delta  "200"}
          yesterday-results (events/add-event-to-flights-log yesterday-event events)
          last-week-event   {:plane-id    "F222"
                             :model       "747"
                             :origin      "DUBLIN"
                             :destination "LONDON"
                             :event-type  "Re-Fuel"
                             :timestamp   yesterday
                             :fuel-delta  "300"}
          final-results     (events/add-event-to-flights-log last-week-event yesterday-results)
          valid-results     (filter #(= "OK" (:status %)) final-results)
          invalid-results   (filter #(= "INVALID" (:status %)) final-results)]
      (t/is (= 1 (count valid-results)))
      (t/is (= 1 (count invalid-results)))
      (t/is (= "300" (:fuel-status (first final-results)))))))

(t/deftest set-status-test
  (t/testing "should assoc INVALID to existing event in events if it is overwritten by new-event"
    (let [old-events [{:timestamp now :hint "to be invalidated" :status "OK"}
                      {:timestamp last-month :hint "will remain valid" :status "OK"}]
          new-event  {:timestamp now :hint "will invalidate the older 'now' event"}]
      (t/is (= [{:timestamp now :hint "to be invalidated" :status "INVALID"}
                {:timestamp last-month :hint "will remain valid" :status "OK"}
                {:timestamp now :hint "will invalidate the older 'now' event" :status "OK"}]
               (events/add-event-to-events
                 new-event
                 old-events))))))

(t/deftest update-maps-in-collection-test
  (t/testing "should update map in a collection if kv matches"
    (let [input      [{:hello "world"}]
          expected   [{:hello "world" :status "INVALID"}]
          updater-fn (fn [m]
                       (assoc m :status "INVALID"))]
      (t/is (= expected
               (events/update-maps-in-collection input
                                                 :hello
                                                 "world"
                                                 updater-fn))))))



