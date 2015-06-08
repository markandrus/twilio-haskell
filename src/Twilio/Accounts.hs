{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RankNTypes #-}

module Twilio.Accounts
  ( -- * Resource
    Accounts(..)
  , Twilio.Accounts.get
  , Twilio.Accounts.post
  , createSubAccount
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding

import Control.Monad.Twilio
import Twilio.Account
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

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

instance Get0 Accounts where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest' "/Accounts.json"

{- | Get 'Accounts'.

For example, you can fetch the 'Accounts' resource in the 'IO' monad as follows:

>module Main where
>
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.Accounts as Accounts
>import Twilio.Types
>
>-- | Print accounts.
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN")
>     $ Accounts.get >>= liftIO . print
-}
get :: MonadThrow m => TwilioT m Accounts
get = Resource.get

instance Post0 Account where
  post0
    = request parseJSONFromResponse =<< makeTwilioPOSTRequest' "/Accounts.json" []

instance Post1 Text Account where
  post1 friendlyName
    = request parseJSONFromResponse =<< makeTwilioPOSTRequest' "/Accounts.json"
      [ ("FriendlyName", encodeUtf8 friendlyName ) ]

instance Post1 (Maybe Text) Account where
  post1 Nothing = post0
  post1 (Just friendlyName) = post1 friendlyName

post :: MonadThrow m => Maybe Text -> TwilioT m Account
post = Resource.post

{- | Create a new 'Account' instance resource as a subaccount of the one used
to make the request.

For example, you can create a subaccount, "foo", as follows:

>module Main where
>
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.Accounts (createSubAccount)
>import Twilio.Types
>
>-- | Create and print a subaccount, "foo".
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN")
>     $ createSubAccount (Just "foo") >>= liftIO . print
-}
createSubAccount :: MonadThrow m
                 => Maybe Text  -- ^ A human readable description of the new
                                -- subaccount, up to 64 characters. Defaults
                                -- to "SubAccount Created at {YYYY-MM-DD
                                -- HH:MM meridian}".
                 -> TwilioT m Account
createSubAccount = Twilio.Accounts.post
