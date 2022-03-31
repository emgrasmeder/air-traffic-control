# Air Traffic Control
This is a little program to find the status of a given set of flights

## Use
To interact with the code, you need to have the clojure cli. You can download it [here](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)

Then you'll need an editor connected to a REPL. I use [Conjure](https://github.com/Olical/conjure/wiki/Quick-start:-Clojure). 
 and start the REPL, then use <leader>ee in front of any s-expression to execute the code. 

 
You can play around with the code in either test/air-traffic-control/core-test 
or in the file src/air-traffic-control/core.clj
At the bottom of that core.clj file, there's an example in a `(comment)` function. 
Your two main functions to use are:
 - `air-traffic-control/core.parse-events!`, which adds events to an atom containing the entire program state and
 - `air-traffic-control/core.fetch-status-at` which takes a timestamp, and searches through that atom to return results

 
## Development
### REPL
Run your nREPL with 
`clj -M:nREPL -m nrepl.cmdline`
### Tests
Run the tests with 
`clj -X:test` 

## Example
 ```
  (with-redefs [immutable-state (atom initial-state)]
    (let [input-events (str
                         "F111 747 start stop Re-Fuel 2000-01-01T00:00:00 100\n"
                         "F2 747 start stop Take-Off 2000-01-01T00:00:00 0\n"
                         "F3 711 start stop Land 2000-01-01T00:00:00 100\n")
          _ (parse-events! input-events)
          result       (fetch-status-at "2022-01-02T00:00:00")]
      (println result))))


; (out) F111 Awaiting-Takeoff 100
; (out) F2 In-Flight 0
; (out) F3 Landed 100
 ```
 
 ## Discussion
There isn't much sad-path handling in this work. I'm not sure what to do if someone gives a non-zero fuel-delta for the take-off event, or exactly how to handle mangled input (if events are missing, or incomplete). I believe this solution, while it has some issues with naming and consistency, is relatively easily adaptapted for better handling of corner cases. In a client scenario, I'd need to get answers from the client or have some business oriented discussions about how to handle some of the sad paths. 
