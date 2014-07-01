{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Accounts
  ( -- * Resource
    Accounts(..)
  , get
  ) where

import Twilio.Types
import Twilio.Account hiding (get)

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

{- Resource -}

data Accounts = Accounts
  { pagingInformation :: !PagingInformation
  , list              :: ![Account]
  } deriving (Show, Eq, Ord)

instance List Accounts Account where
  getListWrapper = wrap (Accounts . fromJust)
  getList = list
  getPlural = Const "accounts"

instance FromJSON Accounts where
  parseJSON = parseJSONToList

-- | Get 'Accounts'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Accounts
get = request "/Accounts.json"
