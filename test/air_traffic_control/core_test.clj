(ns air-traffic-control.core-test
  (:require [clojure.test :as t]
            [air-traffic-control.core :as atc]
            [clj-time.core :as clj-time]
            [clj-time.local :as l]
            [clj-time.coerce :as c]
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
    (let [refueling-yesterday-event {:timestamp   yesterday
                                     :plane-id    "F111"
                                     :event-type  "Re-Fuel"
                                     :fuel-status "200"
                                     :status      "OK"}
          flights                   {:flights {"F111" [refueling-yesterday-event]}}]
      (t/is (= "" (atc/fetch-status-at flights last-week)))))
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
                   (nth 2)))))
    (t/testing "should set fuel status regardless for multiple flights "
      (let [refueling-last-month-event {:timestamp   last-month
                                        :plane-id    "F111"
                                        :event-type  "Re-Fuel"
                                        :fuel-status "428"
                                        :status      "OK"}
            take-off-last-week-event {:timestamp   last-week
                                      :plane-id    "F111"
                                      :event-type  "Take-Off"
                                      :fuel-status "0"
                                      :status      "OK"}
            arrive-yesterday-event {:timestamp   yesterday
                                    :plane-id    "F111"
                                    :event-type  "Land"
                                    :fuel-status "-324"
                                    :status      "OK"}
            flights {:flights {"F111" [arrive-yesterday-event
                                       take-off-last-week-event
                                       refueling-last-month-event]
                               "F222" [(assoc arrive-yesterday-event :plane-id "F222")
                                       (assoc take-off-last-week-event :plane-id "F222")
                                       (assoc refueling-last-month-event :plane-id "F222")]}}]
        (t/is (= "F111 Landed 104\nF222 Landed 104" (atc/fetch-status-at flights now)))))




    (t/testing "should ignore invalidated records"
      (let [invalid-refueling-last-month-event {:timestamp   last-month
                                                :plane-id    "F111"
                                                :event-type  "Re-Fuel"
                                                :fuel-status "9999999"
                                                :status      "INVALID"}
            refueling-last-month-event         {:timestamp   last-month
                                                :plane-id    "F111"
                                                :event-type  "Re-Fuel"
                                                :fuel-status "428"
                                                :status      "OK"}
            take-off-last-week-event           {:timestamp   last-week
                                                :plane-id    "F111"
                                                :event-type  "Take-Off"
                                                :fuel-status "0"
                                                :status      "OK"}
            arrive-yesterday-event             {:timestamp   yesterday
                                                :plane-id    "F111"
                                                :event-type  "Land"
                                                :fuel-status "-324"
                                                :status      "OK"}
            flights                            {:flights {"F111" [invalid-refueling-last-month-event
                                                                  arrive-yesterday-event
                                                                  take-off-last-week-event
                                                                  refueling-last-month-event]}}]
        (t/is (= "F111 Landed 104" (atc/fetch-status-at flights now)))))))


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

(t/deftest parse-events!-test
  (t/testing "should only maintain one list of flights"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [updates [[:update
                      {:plane-id    "F222"
                       :model       "747"
                       :origin      "DUBLIN"
                       :destination "LONDON"
                       :event-type  "Re-Fuel"
                       :timestamp   "2021-03-29T10:00:00"
                       :fuel-delta  "200"}]
                     [:update
                      {:plane-id    "F551"
                       :model       "747"
                       :origin      "PARIS"
                       :destination "LONDON"
                       :event-type  "Re-Fuel"
                       :timestamp   "2021-03-29T10:00:00"
                       :fuel-delta  "345"}]
                     [:update
                      {:plane-id    "F324"
                       :model       "313"
                       :origin      "LONDON"
                       :destination "NEWYORK"
                       :event-type  "Take-Off"
                       :timestamp   "2021-03-29T12:00:00"
                       :fuel-delta  "0"}]
                     [:update
                      {:plane-id    "F123"
                       :model       "747"
                       :origin      "LONDON"
                       :destination "CAIRO"
                       :event-type  "Re-Fuel"
                       :timestamp   "2021-03-29T10:00:00"
                       :fuel-delta  "428"}]
                     [:update
                      {:plane-id    "F123"
                       :model       "747"
                       :origin      "LONDON"
                       :destination "CAIRO"
                       :event-type  "Take-Off"
                       :timestamp   "2021-03-29T12:00:00"
                       :fuel-delta  "0"}]
                     [:update
                      {:plane-id    "F551"
                       :model       "747"
                       :origin      "PARIS"
                       :destination "LONDON"
                       :event-type  "Take-Off"
                       :timestamp   "2021-03-29T11:00:00"
                       :fuel-delta  "0"}]
                     [:update
                      {:plane-id    "F551"
                       :model       "747"
                       :origin      "PARIS"
                       :destination "LONDON"
                       :event-type  "Land"
                       :timestamp   "2021-03-29T12:00:00"
                       :fuel-delta  "-120"}]
                     [:update
                      {:plane-id    "F123"
                       :model       "747"
                       :origin      "LONDON"
                       :destination "CAIRO"
                       :event-type  "Land"
                       :timestamp   "2021-03-29T14:00:00"
                       :fuel-delta  "-324"}]]]
        (run! atc/update-event-log updates)
        (t/is (= 4
                 (-> @atc/immutable-state
                     :state
                     :flights
                     keys
                     count)))))))



(t/deftest end-to-end-test
  (t/testing "should do the first example from the assignment"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [input-events    (str "F222 747 DUBLIN LONDON Re-Fuel 2021-03-29T10:00:00 200\n"
                                 "F551 747 PARIS LONDON Re-Fuel 2021-03-29T10:00:00 345\n"
                                 "F324 313 LONDON NEWYORK Take-Off 2021-03-29T12:00:00 0\n"
                                 "F123 747 LONDON CAIRO Re-Fuel 2021-03-29T10:00:00 428\n"
                                 "F123 747 LONDON CAIRO Take-Off 2021-03-29T12:00:00 0\n"
                                 "F551 747 PARIS LONDON Take-Off 2021-03-29T11:00:00 0\n"
                                 "F551 747 PARIS LONDON Land 2021-03-29T12:00:00 -120\n"
                                 "F123 747 LONDON CAIRO Land 2021-03-29T14:00:00 -324")
            expected-output (str "F123 Landed 104\n"
                                 "F222 Awaiting-Takeoff 200\n"
                                 "F324 In-Flight 0\n"
                                 "F551 Landed 225")]
        (atc/parse-events! input-events)
        (t/is (= (-> expected-output
                     (clj-str/split #"\n")
                     set)
                 (-> (atc/fetch-status-at (:state @atc/immutable-state)
                                          (c/to-date-time "2021-03-29T15:00:00"))
                     (clj-str/split #"\n")
                     set))))))
  (t/testing "should recieve updates"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [input-events    (str "F222 747 DUBLIN LONDON Re-Fuel 2021-03-29T10:00:00 200\n"
                                 "F551 747 PARIS LONDON Re-Fuel 2021-03-29T10:00:00 345\n"
                                 "F324 313 LONDON NEWYORK Take-Off 2021-03-29T12:00:00 0\n"
                                 "F123 747 LONDON CAIRO Re-Fuel 2021-03-29T10:00:00 428\n"
                                 "F123 747 LONDON CAIRO Take-Off 2021-03-29T12:00:00 0\n"
                                 "F551 747 PARIS LONDON Take-Off 2021-03-29T11:00:00 0\n"
                                 "F551 747 PARIS LONDON Land 2021-03-29T12:00:00 -120\n"
                                 "F123 747 LONDON CAIRO Land 2021-03-29T14:00:00 -324")
            expected-output (str "F123 Landed 104\n"
                                 "F222 Awaiting-Takeoff 200\n"
                                 "F324 In-Flight 0\n"
                                 "F551 Landed 45")
            update-msg      "F551 747 PARIS LONDON Land 2021-03-29T12:00:00 -300"]
        (atc/parse-events! input-events)
        (atc/parse-events! update-msg)
        (t/is (= (-> expected-output
                     (clj-str/split #"\n")
                     set)
                 (-> (atc/fetch-status-at (:state @atc/immutable-state)
                                          (c/to-date-time "2021-03-29T15:00:00"))
                     (clj-str/split #"\n")
                     set))))))

  (t/testing "should invalidate events to remove events from log"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [input-events    (str "F222 747 DUBLIN LONDON Re-Fuel 2021-03-29T10:00:00 200\n"
                                 "F551 747 PARIS LONDON Re-Fuel 2021-03-29T10:00:00 345\n"
                                 "F324 313 LONDON NEWYORK Take-Off 2021-03-29T12:00:00 0\n"
                                 "F123 747 LONDON CAIRO Re-Fuel 2021-03-29T10:00:00 428\n"
                                 "F123 747 LONDON CAIRO Take-Off 2021-03-29T12:00:00 0\n"
                                 "F551 747 PARIS LONDON Take-Off 2021-03-29T11:00:00 0\n"
                                 "F551 747 PARIS LONDON Land 2021-03-29T12:00:00 -120\n"
                                 "F123 747 LONDON CAIRO Land 2021-03-29T14:00:00 -324")
            expected-output (str "F123 Landed 104\n"
                                 "F222 Awaiting-Takeoff 200\n"
                                 "F324 In-Flight 0\n"
                                 "F551 In-Flight 345\n")
            update-msg      "F551 2021-03-29T12:00:00"]
        (atc/parse-events! input-events)
        (atc/parse-events! update-msg)
        (t/is (= (-> expected-output
                     (clj-str/split #"\n")
                     set)
                 (-> (atc/fetch-status-at (:state @atc/immutable-state)
                                          (c/to-date-time "2021-03-29T15:00:00"))
                     (clj-str/split #"\n")
                     set)))))))



(t/deftest get-status-for-flight-test
  (t/testing "should not cut off the fuel amount even if there are duplicates"
    (let [input           [{:timestamp   (c/to-date-time "2022-02-18T14:58:30.628Z")
                            :plane-id    "F111"
                            :event-type  "Re-Fuel"
                            :fuel-status "9999999"
                            :status      "INVALID"}
                           {:timestamp   (c/to-date-time "2022-03-29T14:58:30.628Z")
                            :plane-id    "F111"
                            :event-type  "Land"
                            :fuel-status "-324"
                            :status      "OK"}
                           {:timestamp   (c/to-date-time "2022-03-22T14:58:30.628Z")
                            :plane-id    "F111"
                            :event-type  "Take-Off"
                            :fuel-status "0"
                            :status      "OK"}
                           {:timestamp   (c/to-date-time "2022-02-18T14:58:30.628Z")
                            :plane-id    "F111"
                            :event-type  "Re-Fuel"
                            :fuel-status "428"
                            :status      "OK"}]
          unmapped-output ((partial atc/get-status-for-flight now) input)]
      (t/is (= "F111 Landed 104" unmapped-output)))))

(t/deftest calculate-fuel-diff
  (t/testing "should give us the correct fuel diff"
    (let [events [{:timestamp   (c/to-date-time "2022-02-18T14:58:30.628Z")
                   :plane-id    "F111"
                   :event-type  "Re-Fuel"
                   :fuel-status "9999999"
                   :status      "INVALID"}
                  {:timestamp   (c/to-date-time "2022-03-29T14:58:30.628Z")
                   :plane-id    "F111"
                   :event-type  "Land"
                   :fuel-status "-324"
                   :status      "OK"}
                  {:timestamp   (c/to-date-time "2022-03-22T14:58:30.628Z")
                   :plane-id    "F111"
                   :event-type  "Take-Off"
                   :fuel-status "0"
                   :status      "OK"}
                  {:timestamp   (c/to-date-time "2022-02-18T14:58:30.628Z")
                   :plane-id    "F111"
                   :event-type  "Re-Fuel"
                   :fuel-status "428"
                   :status      "OK"}]
          result (atc/calculate-fuel-diff events)]
      (t/is (= 104 (:fuel-status result))))))


(t/deftest all-combinations-test
  (t/testing "should work for single events"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [input-events (str
                           "F1-Refuel 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
                           "F2-TakeOff 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
                           "F3-Land 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n")
            _ (atc/parse-events! input-events)
            result       (atc/fetch-status-at "2022-01-02T00:00:00")]
        (t/is
          (= (str
               "F1-Refuel Awaiting-Takeoff 100\n"
               "F2-TakeOff Awaiting-Takeoff 100\n"
               "F3-Land Awaiting-Takeoff 100")
             result)))))
  (t/testing "should work for all combinations of two events"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [input-events     (str
                               "F12-RefuelTakeOff 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
                               "F12-RefuelTakeOff 747 start stop Take-Off 2001-01-01T00:00:00 0\n"

                               "F13-RefuelLand 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
                               "F13-RefuelLand 747 start stop Land 2001-01-01T00:00:00 200\n"

                               "F21-TakeOffRefuel 747 start stop Take-Off 2000-01-01T00:00:00 100\n"
                               "F21-TakeOffRefuel 747 start stop Re-Fuel 2001-01-01T00:00:00 200\n"

                               "F31-LandRefuel 747 start stop Land 2000-01-01T00:00:00 100\n"
                               "F31-LandRefuel 747 start stop Re-Fuel 2001-01-01T00:00:00 200\n"

                               "F23-TakeOffLand 747 start stop Take-Off 2000-01-01T00:00:00 100\n"
                               "F23-TakeOffLand 747 start stop Land 2001-01-01T00:00:00 200\n"

                               "F32-LandTakeOff 747 start stop Land 2000-01-01T00:00:00 100\n"
                               "F32-LandTakeOff 747 start stop Take-Off 2001-01-01T00:00:00 0\n")
            expected-results (str
                               "F12-RefuelTakeOff In-Flight 100\n"
                               "F13-RefuelLand Landed 200\n"
                               "F21-TakeOffRefuel Awaiting-Takeoff 200\n"
                               "F31-LandRefuel Awaiting-Takeoff 200\n"
                               "F23-TakeOffLand Landed 200\n"
                               "F32-LandTakeOff In-Flight 100")


            _ (atc/parse-events! input-events)
            result           (atc/fetch-status-at "2022-01-02T00:00:00")]
        (t/is (= (-> expected-results
                     (clj-str/split #"\n")
                     set)
                 (-> result
                     (clj-str/split #"\n")
                     set))))))
  (t/testing "should work for all combinations of 3 events"
    (with-redefs [atc/immutable-state (atom atc/initial-state)]
      (let [input-events
            (str
              "F123-RefuelTakeOffLand 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
              "F123-RefuelTakeOffLand 747 start stop Take-Off 2001-01-01T00:00:00 200\n"
              "F123-RefuelTakeOffLand 747 start stop Land 2002-01-01T00:00:00 -300\n"

              "F132-RefuelLandTakeOff 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
              "F132-RefuelLandTakeOff 747 start stop Land 2001-01-01T00:00:00 200\n"
              "F132-RefuelLandTakeOff 747 start stop Take-Off 2002-01-01T00:00:00 0\n"

              "F213-TakeOffRefuelLand 747 start stop Take-Off 2000-01-01T00:00:00 100\n"
              "F213-TakeOffRefuelLand 747 start stop Re-Fuel 2001-01-01T00:00:00 200\n"
              "F213-TakeOffRefuelLand 747 start stop Land 2002-01-01T00:00:00 300\n"

              "F231-TakeOffLandRefuel 747 start stop Take-Off 2000-01-01T00:00:00 100\n"
              "F231-TakeOffLandRefuel 747 start stop Land 2001-01-01T00:00:00 200\n"
              "F231-TakeOffLandRefuel 747 start stop Re-Fuel 2002-01-01T00:00:00 300\n"

              "F312-LandRefuelTakeOff 747 start stop Land 2000-01-01T00:00:00 100\n"
              "F312-LandRefuelTakeOff 747 start stop Re-Fuel 2001-01-01T00:00:00 200\n"
              "F312-LandRefuelTakeOff 747 start stop Take-Off 2002-01-01T00:00:00 0\n"

              "F321-LandTakeOffRefuel 747 start stop Land 2000-01-01T00:00:00 100\n"
              "F321-LandTakeOffRefuel 747 start stop Take-Off 2001-01-01T00:00:00 200\n"
              "F321-LandTakeOffRefuel 747 start stop Re-Fuel 2002-01-01T00:00:00 300\n")


            expected-results (str
                               "F123-RefuelTakeOffLand Landed -200\n"
                               "F132-RefuelLandTakeOff In-Flight 200\n"
                               "F213-TakeOffRefuelLand Landed 300\n"
                               "F231-TakeOffLandRefuel Awaiting-Takeoff 300\n"
                               "F312-LandRefuelTakeOff In-Flight 200\n"
                               "F321-LandTakeOffRefuel Awaiting-Takeoff 300")


            _ (atc/parse-events! input-events)
            result           (atc/fetch-status-at "2022-01-02T00:00:00")]
        (t/is (= (-> expected-results
                     (clj-str/split #"\n")
                     set)
                 (-> result
                     (clj-str/split #"\n")
                     set)))))))

