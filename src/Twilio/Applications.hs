{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Applications
  ( -- * Resource
    Applications(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.Application hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe

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

-- | Get the 'Applications' for your account.
get :: (MonadThrow m, MonadIO m) => TwilioT m Applications
get = requestForAccount "/Applications.json"

-- | Get the 'Applications' for a sub-account of your account.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m Applications
get' = flip forAccount get
