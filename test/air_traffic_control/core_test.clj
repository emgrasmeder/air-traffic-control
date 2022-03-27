(ns air-traffic-control.core-test
  (:require [clojure.test :as t]
            [air-traffic-control.core :as atc]
            [clj-time.core :as clj-time]
            [air-traffic-control.events :as events]
            [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]))

(def now (clj-time/now))
(def yesterday (clj-time/minus now (clj-time/days 1)))
(def tomorrow (clj-time/plus now (clj-time/days 1)))
(def last-week (clj-time/minus now (clj-time/days 8)))
(def last-month (clj-time/minus now (clj-time/days 40)))

(t/deftest parse-airplane-status-test
  (t/testing "should validate basic types properly"
    (t/is (s/valid? ::events/timestamp "2020-03-03T10:00:00"))
    (t/is (s/valid? ::events/plane-id "F222"))
    (t/is (s/valid? ::events/removal
                    (clj-str/split "F222 2020-03-03T10:00:00" #" ")))
    (t/is (s/valid? ::events/event
                    (clj-str/split "F222 2020-03-03T10:00:00" #" ")))
    (t/is (s/valid? ::events/event
                    (clj-str/split "F222 2020-03-03T10:00:00" #" "))))
  (t/testing "should recognize a removal as a valid event"
    (t/is (= [:removal
              {:plane-id "F222" :timestamp "2021-03-29T10:00:00"}]
             (atc/parse-event "F222 2021-03-29T10:00:00"))))
  (t/testing
    "should recognize an update as a valid event"
    (t/is (= [:update
              {:plane-id    "F222"
               :model       "747"
               :origin      "DUBLIN"
               :destination "LONDON"
               :event-type  "Re-Fuel"
               :timestamp   "2021-03-29T10:00:00"
               :fuel-delta  "200"}]
             (atc/parse-event
               "F222 747 DUBLIN LONDON Re-Fuel 2021-03-29T10:00:00 200")))))


(t/deftest sort-map-test
  (t/testing "should sort a map by timestamp key"
    (let [flights         {:flight1 {:timestamp "2021-03-29T10:00:00"}
                           :flight2 {:timestamp "2021-04-29T10:00:00"}
                           :flight3 {:timestamp "2021-05-29T10:00:00"}
                           :flight4 {:timestamp "2020-05-29T10:00:00"}
                           :flight5 {:timestamp "2019-05-29T10:00:00"}
                           :flight6 {:timestamp "1999-11-11T11:11:11"}
                           :flight7 {:timestamp "2000-05-29T10:00:00"}}
          earliest-flight (first (atc/sort-map-by flights :timestamp))]
      (t/is (= earliest-flight
               [:flight6 {:timestamp "1999-11-11T11:11:11"}])))))


(t/deftest update-event-log-test
  (t/testing
    "should recognize an update as a valid event"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [result1 (-> "F111 000 DUBLIN LONDON Re-Fuel 1111-01-01T00:00:00 200"
                        (atc/parse-event)
                        (atc/update-event-log))
            result2 (-> "F222 000 DUBLIN LONDON Re-Fuel 2222-01-01T00:00:00 200"
                        (atc/parse-event)
                        (atc/update-event-log))
            result3 (-> "F222 000 DUBLIN LONDON Re-Fuel 1111-01-01T00:00:00 200"
                        (atc/parse-event)
                        (atc/update-event-log))]
        (t/is (= result1
                 {:state {:flights {"F111" [{:timestamp   "1111-01-01T00:00:00"
                                             :plane-id    "F111"
                                             :event-type  "Re-Fuel"
                                             :fuel-status "200"
                                             :status      "OK"}]}}}))
        (t/is (= result2
                 {:state {:flights {"F111" [{:timestamp   "1111-01-01T00:00:00"
                                             :plane-id    "F111"
                                             :event-type  "Re-Fuel"
                                             :fuel-status "200"
                                             :status      "OK"}]
                                    "F222" [{:timestamp   "2222-01-01T00:00:00"
                                             :plane-id    "F222"
                                             :event-type  "Re-Fuel"
                                             :fuel-status "200"
                                             :status      "OK"}]}}}))
        (t/is (= result3
                 {:state {:flights {"F111" [{:timestamp   "1111-01-01T00:00:00"
                                             :plane-id    "F111"
                                             :event-type  "Re-Fuel"
                                             :fuel-status "200"
                                             :status      "OK"}]
                                    "F222" [{:timestamp   "2222-01-01T00:00:00"
                                             :plane-id    "F222"
                                             :event-type  "Re-Fuel"
                                             :fuel-status "200"
                                             :status      "OK"}
                                            {:timestamp   "1111-01-01T00:00:00"
                                             :plane-id    "F222"
                                             :event-type  "Re-Fuel"
                                             :fuel-status "200"
                                             :status      "OK"}]}}}))))))

(t/deftest remove-timestamps-after-test
  (t/testing "should remove timestamps after time-query from collection1"
    (let [events [{:timestamp last-week}
                  {:timestamp now}]]
      (t/is (= [{:timestamp last-week}]
               (atc/remove-timestamps-after events yesterday))))))


(t/deftest fetch-status-test
  (t/testing "should not find flight events that happened after the requested time"
    ; THIS ONE ALMOST WORKS BUT WE NEED TO PARSE IF THERES NO DATA
    (let [refueling-yesterday-event {:timestamp   yesterday
                                     :plane-id    "F111"
                                     :event-type  "Re-Fuel"
                                     :fuel-status "200"
                                     :status      "OK"}
          flights                   {:flights {"F111" [refueling-yesterday-event]}}]
      (t/is (= nil (atc/fetch-status-at flights last-week)))))
  (t/testing
    "should fetch status 'Awaiting-Takeoff' for flights who most recently refueled after a given timestamp"
    (let [refueling-yesterday-event {:timestamp   yesterday
                                     :plane-id    "F111"
                                     :event-type  "Re-Fuel"
                                     :fuel-status "200"
                                     :status      "OK"}
          flights                   {:flights {"F111" [refueling-yesterday-event]}}]
      (t/is (= "Awaiting-Takeoff"
               (-> (atc/fetch-status-at flights now)
                   (clj-str/split #" ")
                   second)))))
  (t/testing
    "should fetch status 'F111' for flight F111"
    (let [refueling-yesterday-event {:timestamp   yesterday
                                     :plane-id    "F111"
                                     :event-type  "Re-Fuel"
                                     :fuel-status "200"
                                     :status      "OK"}
          flights                   {:flights {"F111" [refueling-yesterday-event]}}]
      (t/is (= "F111"
               (-> (atc/fetch-status-at flights now)
                   (clj-str/split #" ")
                   first)))))
  (t/testing "should set fuel status to 200 after refuel"
    (let [refueling-yesterday-event {:timestamp   yesterday
                                     :plane-id    "F111"
                                     :event-type  "Re-Fuel"
                                     :fuel-status "200"
                                     :status      "OK"}
          flights                   {:flights {"F111" [refueling-yesterday-event]}}]
      (t/is (= "200"
               (-> (atc/fetch-status-at flights now)
                   (clj-str/split #" ")
                   (nth 2))))))
  (t/testing "should set fuel status to 0 after a takeoff"
    (let [refueling-yesterday-event {:timestamp   yesterday
                                     :plane-id    "F111"
                                     :event-type  "Take-Off"
                                     :fuel-status "0"
                                     :status      "OK"}
          flights                   {:flights {"F111" [refueling-yesterday-event]}}]
      (t/is (= "0"
               (-> (atc/fetch-status-at flights now)
                   (clj-str/split #" ")
                   (nth 2))))))
  (t/testing "should set fuel status to the delta between refueling and landing after arrival"
    (let [refueling-last-month-event {:timestamp   last-month
                                      :plane-id    "F111"
                                      :event-type  "Re-Fuel"
                                      :fuel-status "428"
                                      :status      "OK"}
          take-off-last-week-event   {:timestamp   last-week
                                      :plane-id    "F111"
                                      :event-type  "Take-Off"
                                      :fuel-status "0"
                                      :status      "OK"}
          arrive-yesterday-event     {:timestamp   yesterday
                                      :plane-id    "F111"
                                      :event-type  "Land"
                                      :fuel-status "-324"
                                      :status      "OK"}
          flights                    {:flights {"F111" [arrive-yesterday-event
                                                        take-off-last-week-event
                                                        refueling-last-month-event]}}]
      (t/is (= "104"
               (-> (atc/fetch-status-at flights now)
                   (clj-str/split #" ")
                   (nth 2)))))))

(t/deftest calculate-fuel-diff-test
  (t/testing "should compute the fuel level given a refuel, departure, and arrival"
    (let [refueling-last-month-event {:timestamp   last-month
                                      :plane-id    "F111"
                                      :event-type  "Re-Fuel"
                                      :fuel-status "428"
                                      :status      "OK"}
          take-off-last-week-event   {:timestamp   last-week
                                      :plane-id    "F111"
                                      :event-type  "Take-Off"
                                      :fuel-status "0"
                                      :status      "OK"}
          arrive-yesterday-event     {:timestamp   yesterday
                                      :plane-id    "F111"
                                      :event-type  "Land"
                                      :fuel-status "-324"
                                      :status      "OK"}
          events                     [arrive-yesterday-event
                                      take-off-last-week-event
                                      refueling-last-month-event]]
      (t/is (= 104 (:fuel-status (atc/calculate-fuel-diff events)))))))



(def events
  ["F222 747 DUBLIN LONDON Re-Fuel 2021-03-29T10:00:00 200"
   "F551 747 PARIS LONDON Re-Fuel 2021-03-29T10:00:00 345"
   "F324 313 LONDON NEWYORK Take-Off 2021-03-29T12:00:00 0"
   "F123 747 LONDON CAIRO Re-Fuel 2021-03-29T10:00:00 428"
   "F123 747 LONDON CAIRO Take-Off 2021-03-29T12:00:00 0"
   "F551 747 PARIS LONDON Take-Off 2021-03-29T11:00:00 0"
   "F551 747 PARIS LONDON Land 2021-03-29T12:00:00 -120"
   "F123 747 LONDON CAIRO Land 2021-03-29T14:00:00 -324"])

(def expected-events
  ["F123 Landed 104" "F222 Awaiting-Takeoff 200" "F324 In-Flight 0"
   "F551 Landed 225"])



