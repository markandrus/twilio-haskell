twilio
======

This package provides a library for interacting with
[Twilio's API](www.twilio.com/docs/api). Install using

```
$ cabal install twilio
```

Example
-------

You can create a REST API client and fetch the calls resources as follows

```hs
import           System.Environment
import qualified Twilio.Call        as Call
import           Twilio.Types

main :: IO ()
  accountSID <- getEnv "ACCOUNT_SID"
  authToken  <- getEnv "AUTH_TOKEN"
  let Just credentials = parseCredentials accountSID authToken
  runTwilio credentials $ do
    calls <- Call.calls
    liftIO $ print calls
```

TODO
----

* Continue adding resources.
* Eventually separate resources from the methods used to retrieve the resources?
* Add query filters for lists.
* Add subresources.
