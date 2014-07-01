module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import qualified Twilio.Call as Call
import Twilio.Types

-- | Print calls.
main :: IO ()
main = runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN")
     $ Call.calls' >>= liftIO . print
