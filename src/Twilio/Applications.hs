{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Applications
  ( -- * Resource
    Applications(..)
  , Twilio.Applications.get
  ) where

import Twilio.Types
import Twilio.Application hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data Applications = Applications
  { applicationsPagingInformation :: PagingInformation
  , applicationList :: [Application]
  } deriving (Show, Eq)

instance List Applications Application where
  getListWrapper = wrap (Applications . fromJust)
  getList = applicationList
  getPlural = Const "applications"

instance FromJSON Applications where
  parseJSON = parseJSONToList

instance Get0 Applications where
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest "/Applications.json"

{- | Get the 'Applications' for your account.

For example, you can fetch the 'Applications' resource in the 'IO' monad as follows:

>module Main where
>
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.Applications as Applications
>import Twilio.Types
>
>-- | Print applications.
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN")
>     $ Applications.get >>= liftIO . print
-}
get :: (MonadThrow m, MonadIO m) => TwilioT m Applications
get = Resource.get
