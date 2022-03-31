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

