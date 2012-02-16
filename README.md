#NetSpec
**A Haskell library to simplify certain Networking tasks**

Certain networking tasks have a very simple format:

* Set up a given number of connections and initial state
* Loop until some condition is met
* Wrap up and close connections

NetSpec provides a simple way to write such tasks.

See the examples for how to use NetSpec.
More examples soon to come. The general way to use NetSpec is to pick
whether you are a client or a server, and which format you want to 
deal with. You do this with imports.

To select which format you want to deal with, you have 3 options:
ByteString.Char8, Text, or JSON. JSON is recommended; you can
derive the necessary typeclass instances with `$(deriveJson id ''MyDataType)`

    {-# LANGUAGE OverloadedStrings #-}
    import Network.NetSpec.ByteString

      -- or --

    {-# LANGUAGE OverloadedStrings #-}
    import Network.NetSpec.Text

      -- or --

    {-# LANGUAGE TemplateHaskell #-}
    import Network.NetSpec.Json

You select whether you are a server or a client like so:

    import Network.NetSpec.Server

      -- or --

    import Network.NetSpec.Client

The difference between a server and a client is that
the server listens and waits for connections, while the client
immediately demands connections.

To run the Echo example, open 3 terminals:

    0$ runhaskell Network/NetSpec/Examples/Echo.hs
    1$ telnet localhost 5001
    2$ telnet localhost 5002

Type things into terminal 1, and watch as they are relayed to terminal 2.
