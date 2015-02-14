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

Contributing
------------

Feel free to contribute to any of the open [issues]
(https://github.com/markandrus/twilio-haskell/issues), bugfixes, etc. When you
think you're ready to merge, ensure the tests are passing and open a pull
request. If you are adding new functionality, please include new tests as well.
Finally, add yourself to the `AUTHORS` file.
