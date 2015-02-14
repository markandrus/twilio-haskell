{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Accounts
  ( -- * Resource
    Accounts(..)
  , get
  , createSubAccount
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
get :: (MonadThrow m, MonadIO m) => TwilioT m Accounts
get = request "/Accounts.json"

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
createSubAccount :: (MonadThrow m, MonadIO m)
                 => Maybe String  -- ^ A human readable description of the new
                                  -- subaccount, up to 64 characters. Defaults
                                  -- to "SubAccount Created at {YYYY-MM-DD
                                  -- HH:MM meridian}".
                 -> TwilioT m (Maybe Account)
createSubAccount friendlyName = return Nothing
