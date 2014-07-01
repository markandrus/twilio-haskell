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
module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Twilio.Calls as Calls
import Twilio.Types

-- | Print calls.
main :: IO ()
main = runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN")
     $ Calls.get >>= liftIO . print
```

TODO
----

* Continue adding resources.
* Eventually separate resources from the methods used to retrieve the resources?
* Add query filters for lists.
* Add subresources.
