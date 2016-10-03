{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RankNTypes #-}

module Twilio.APIKeys
  ( -- * Resource
    APIKeys(..)
  , Twilio.APIKeys.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding

import Control.Monad.Twilio
import Twilio.APIKey
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data APIKeys = APIKeys
  { pagingInformation :: !PagingInformation
  , list              :: ![APIKey]
  } deriving (Show, Eq, Ord)

instance List APIKeys APIKey where
  getListWrapper = wrap (APIKeys . fromJust)
  getList = list
  getPlural = Const "keys"

instance FromJSON APIKeys where
  parseJSON = parseJSONToList

instance Get0 APIKeys where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest "/Keys.json"

{- | Get 'APIKeys'.

For example, you can fetch the 'APIKeys' resource in the 'IO' monad as follows:

>module Main where
>
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.APIKeys as APIKeys
>import Twilio.Types
>
>-- | Print API Keys.
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN")
>     $ APIKeys.get >>= liftIO . print
-}
get :: MonadThrow m => TwilioT m APIKeys
get = Resource.get
