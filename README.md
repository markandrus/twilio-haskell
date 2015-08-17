twilio
======

[![twilio on Hackage](https://img.shields.io/hackage/v/twilio.svg)](https://hackage.haskell.org/package/twilio) [![twilio-haskell on Travis CI](https://travis-ci.org/markandrus/twilio-haskell.svg)](https://travis-ci.org/markandrus/twilio-haskell)

This package provides a library for interacting with
[Twilio's API](www.twilio.com/docs/api). Install using

```
$ cabal install twilio
```

Documentation soon to be available on Hackage. For now, see [markandrus.github.io/twilio-haskell](http://markandrus.github.io/twilio-haskell).

For TwiML, see [twiml-haskell](http://github.com/markandrus/twiml-haskell).

Example
-------

You can create a REST API client and fetch the calls resources as follows

```hs
{-#LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Twilio
import Twilio.Calls as Calls
import Twilio.Messages

main :: IO ()
main = runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN") $ do
  -- Print Calls.
  calls <- Calls.get
  liftIO $ print calls

  -- Send a Message.
  let body = PostMessage "+14158059869" "+14158059869" "Oh, hai"
  message <- post body
  liftIO $ print message
```

Contributing
------------

Feel free to contribute to any of the open [issues]
(https://github.com/markandrus/twilio-haskell/issues), bugfixes, etc. When you
think you're ready to merge, ensure the tests are passing and open a pull
request. If you are adding new functionality, please include new tests as well.
Finally, add yourself to the `AUTHORS` file.
