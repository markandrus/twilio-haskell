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
import qualified Twilio.Client as Client
import qualified Twilio.Call as Call

import Data.Maybe (fromJust)
import System.Environment (getEnv)

-- | Construct a REST API client using credentials passed in via environment
-- variables.
client :: IO Client.Client
client = do
  accountSID <- getEnv "ACCOUNT_SID"
  authToken  <- getEnv "AUTH_TOKEN"
  return . fromJust $ Client.client accuntSID authToken

-- | Now, fetch the calls resource.
main :: IO ()
main = do
  accounts <- Call.calls =<< client
  print accounts
```

TODO
----

* Continue adding resources.
* Eventually separate resources from the methods used to retrieve the resources?
* Expose a monad rather than always passing in `Client`.
* Add query filters for lists.
* Add subresources.
