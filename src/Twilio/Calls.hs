{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Calls
  ( -- * Resource
    Calls(..)
  , Twilio.Calls.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Call
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Calls = Calls
  { callsPagingInformation :: PagingInformation
  , callList :: [Call]
  } deriving (Show, Eq)

instance List Calls Call where
  getListWrapper = wrap (Calls . fromJust)
  getList = callList
  getPlural = Const "calls"

instance FromJSON Calls where
  parseJSON = parseJSONToList

instance Get0 Calls where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Calls.json"

{- | Get 'Calls'.

For example, you can fetch the 'Calls' resource in the 'IO' monad as follows:

>module Main where
>
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.Calls as Calls
>import Twilio.Types
>
>-- | Print calls.
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN")
>     $ Calls.get >>= liftIO . print
-}
get :: MonadThrow m => TwilioT m Calls
get = Resource.get
