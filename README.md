#NetSpec
**A Haskell library to simplify certain Networking tasks**

Certain networking tasks have a very simple format:

* Set up a given number of connections and initial state
* Loop until some condition is met
* Wrap up and close connections

NetSpec provides a simple way to write such tasks.

See the Echo example for how to use NetSpec for stateless situations.
More examples soon to come.

To run the example, open 3 terminals:

    0$ runhaskell Network/NetSpec/Examples/Echo.hs
    1$ telnet localhost 5001
    2$ telnet localhost 5002

Type things into terminal 1, and watch as they are relayed to terminal 2.
