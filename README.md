![travis build status](https://travis-ci.org/DanBurton/netspec.svg?branch=master)

#NetSpec
**A Haskell library to simplify static Networking tasks**

Certain networking tasks have a very simple format:

* Set up a given number of connections and initial state
* Loop until some condition is met
* Wrap up and close connections

NetSpec provides a straightforward way to specify and run such tasks,
and also provides convenience functions `!` and `receive`
in the spirit of Erlang's simple message passing.
These two parts of NetSpec can be used separately,
but are designed to fit nicely together.

When you use NetSpec, you choose whether you are a client or a server,
and which message format you want to use.

You have 3 options for message formats:
`ByteString` (Lazy), `Text`, and `JSON`. `Text` uses the newline character
as a sentinel for the end of a message, while `ByteString` and `JSON`
indicate the length of the remaining message in the first 64 bits.
Although `JSON` is built on top of `ByteString`, when you use `JSON`
you can automatically serialize to and from your own data types
for messages, without worrying about `ByteString`s at all.
For this reason, `JSON` is the recommended approach.
You can derive the necessary typeclass instances with
`$(deriveJson id ''MyDataType)`.

Regardless of your choice,
NetSpec attempts to hide the implementation of message-passing from you
(connecting NetSpec clients and NetSpec servers should be a breeze)
but at the same time tries to use a sensible implementation that
can easily work with other programming languages and systems.

    {-# LANGUAGE OverloadedStrings #-}
    import Network.NetSpec.ByteString

      -- or --

    {-# LANGUAGE OverloadedStrings #-}
    import Network.NetSpec.Text

      -- or --

    {-# LANGUAGE TemplateHaskell #-}
    import Network.NetSpec.Json

You select whether you are a server or a client based on
which kind of spec you run.

    import Network.NetSpec

    main = runSpec ServerSpec { ... }

      -- or --

    main = runSpec ClientSpec { ... }

The difference between a server and a client is that
the server specifies ports to listen on, and waits for connections,
while the client specifies hostnames and ports,
and immediately demands connections. However, other than this detail,
servers and clients are specified the same: how to start up,
how to loop, and how to shut down.

The examples are found in the "examples" folder (surprise!).
To run them, you'll need to install the NetSpec library.

    $ pwd
    blah/blah/netspec

    $ cabal install
    blah blah
    Registering netspec-0.1.0.0...

    $ cd examples

Or, assuming you have the Haskell Platform and the aeson library installed,
you don't actually have to install NetSpec to run the examples.
Just use the `-i` option to tell it where the NetSpec source is.

    $ pwd
    blah/blah/netspec/examples

    $ runhaskell -i.:../src SomeExample.hs

To run the Relay example, open 3 terminals:

    0$ runhaskell Relay.hs
    1$ telnet localhost 5001
    2$ telnet localhost 5002

Type things into terminal 1, and watch as they are relayed to terminal 2.
You can run the Chat example in much the same way. These examples illustrate
a stateless server, using `Text` messages. The termination string `"bye\r"`
has that dumb `\r` because that's what telnet sends. Remember, `Text`
uses only `\n` as the end-of-message sentinel, so it is up to you to handle
`\r` if you are dealing with outside programs that send it. (NetSpec does not).

Contrast this with the Echo / Telnet example, which illustrates
how nicely a NetSpec server and NetSpec client can communicate (with Text).

    0$ runhaskell Echo.hs
    1$ runhaskell Telnet.hs localhost 5001

To run the Blackjack example, you'll need the `random-shuffle` package

    $ cabal install random-shuffle

Open 3 terminals:

    0$ runhaskell BlackjackServer.hs
    1$ runhaskell BlackjackClient.hs localhost 5001 bot
    2$ runhaskell BlackjackClient.hs localhost 5002

The Blackjack example illustrates
a stateful server,
mostly statless clients,
and uses `JSON` messages.
It uses some of the StateT-related
convenience functions exported by NetSpec.

<hr />

(c) 2012 Dan Burton

danburton.email@gmail.com
