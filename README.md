twilio
======

[![twilio on Hackage](https://img.shields.io/hackage/v/twilio.svg)](https://hackage.haskell.org/package/twilio) [![twilio-haskell on Travis CI](https://travis-ci.org/markandrus/twilio-haskell.svg)](https://travis-ci.org/markandrus/twilio-haskell)

This package provides a library for interacting with
[Twilio's API](https://www.twilio.com/docs/api). Install using

```
$ cabal install twilio
```

Documentation is available through [GitHub](https://markandrus.github.io/twilio-haskell)
(for HEAD) or [Hackage](https://hackage.haskell.org/package/twilio) for the
current and preceding releases.

For TwiML, see [twiml-haskell](https://github.com/markandrus/twiml-haskell).

Example 1 - using environment variables
-------

You can create a REST API client, fetch the calls, or send a message as follows:

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
  let fromPhone = "+14158059869"
  let toPhone = "+14158059869"
  let body = PostMessage receivingPhone sendingPhone "Oh, hai"
  message <- post body
  liftIO $ print message
```

Example 2 - using SID and secret embedded in the code
-------

You can create a REST API client and send a message as follows:

```hs
{-#LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Twilio
import Twilio.Messages
import Twilio.Types.SID

parseCredentials :: Text -> Text -> Maybe Credentials
parseCredentials accountSID authToken =
  case parseAuthToken authToken of
    Just authToken ->
      (case parseSID accountSID of
        Just accountSID ->
          Just (accountSID, authToken)
        Nothing -> Nothing)
    Nothing -> Nothing

main :: IO ()
main =
  let accountSID = "youraccountSID"
    authToken = "yourauthToken"
  in case parseCredentials accountSID authToken of
    Just credentialsPassed ->
      runTwilio ( credentialsPassed ) $ do
        let fromPhone = "+14158059869"
        let toPhone = "+14158059869"
        let body = PostMessage receivingPhone sendingPhone "Oh, hai"
        message <- post body
        liftIO $ print message
    Nothing -> print "Something bad happened, you have poorly formed credentials."
```

Contributing
------------

Feel free to contribute to any of the open [issues](https://github.com/markandrus/twilio-haskell/issues), bugfixes, etc. When you think you're ready to merge, ensure the tests are passing and open a pull request. If you are adding new functionality, please include new tests as well. Finally, add yourself to the `AUTHORS` file.
