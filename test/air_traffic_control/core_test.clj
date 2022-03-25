(ns air-traffic-control.core-test
  (:require [clojure.test :as t]
            [air-traffic-control.core :as atc]
            [air-traffic-control.events :as events]
            [clojure.spec.alpha :as s]
            [clojure.string :as clj-str]))


(t/deftest parse-airplane-status-test
  (t/testing "should validate basic types properly"
   (t/is (s/valid? ::events/timestamp "2020-03-03T10:00:00"))
   (t/is (s/valid? ::events/plane-id "F222"))
   (t/is (s/valid? ::events/removal (clj-str/split "F222 2020-03-03T10:00:00" #" ")))
   (t/is (s/valid? ::events/event (clj-str/split "F222 2020-03-03T10:00:00" #" ")))
   (t/is (s/valid? ::events/event (clj-str/split "F222 2020-03-03T10:00:00" #" "))))
  (t/testing "should recognize a removal as a valid event"
   (t/is (= [:removal {:plane-id "F222" :timestamp "2021-03-29T10:00:00"}] (atc/parse-event "F222 2021-03-29T10:00:00")))) 
  (t/testing "should recognize an update as a valid event"
   (t/is (= [:update {:plane-id "F222"
                      :model "747"
                      :origin "DUBLIN"
                      :destination "LONDON"
                      :event-type "Re-Fuel"
                      :timestamp "2021-03-29T10:00:00"
                      :fuel-delta "200"}]
           (atc/parse-event "F222 747 DUBLIN LONDON Re-Fuel 2021-03-29T10:00:00 200"))))) 


(t/deftest sort-map-test
  (t/testing "should sort a map by timestamp key"
   (let [flights {:flight1 {:timestamp "2021-03-29T10:00:00"}
                  :flight2 {:timestamp "2021-04-29T10:00:00"}
                  :flight3 {:timestamp "2021-05-29T10:00:00"}
                  :flight4 {:timestamp "2020-05-29T10:00:00"}
                  :flight5 {:timestamp "2019-05-29T10:00:00"}
                  :flight6 {:timestamp "1999-11-11T11:11:11"}
                  :flight7 {:timestamp "2000-05-29T10:00:00"}}
         earliest-flight (first (atc/sort-map-by flights :timestamp))]
         
     (t/is (= earliest-flight [:flight6 {:timestamp "1999-11-11T11:11:11"}])))))

    

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
  [
   "F123 Landed 104"
   "F222 Awaiting-Takeoff 200"
   "F324 In-Flight 0"
   "F551 Landed 225"])


   
