twilio
======

This package provides a library for interacting with
[Twilio's API](www.twilio.com/docs/api). Install using

```
$ cabal install twilio
```

Example
-------

You can create a client for accessing Twilio's API as follows

```hs
import qualified Twilio.Client as Client

client :: Client
client = fromJust $ Client.client "AC123" "45678"
```

TODO
----

* Everything.
