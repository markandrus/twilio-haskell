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
