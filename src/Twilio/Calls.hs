{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Calls
  ( -- * Resource
    Calls(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.Call hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

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

-- | Get 'Calls'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Calls
get = requestForAccount "/Calls.json"

-- | Get an account's 'Calls'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m Calls
get' = flip forAccount get
