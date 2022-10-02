{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Twilio
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Control.Monad.Twilio
  ( -- * The Twilio monad
    Twilio
  , runTwilio
  , runTwilio'
    -- * The Twilio monad transformer
  , TwilioT(..)
  , runTwilioT
  , runTwilioT'
    -- * Types
  , Credentials
  , TwilioException(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Client

import Twilio.Internal.Request
import Twilio.Types.AuthToken
import Twilio.Types.SIDs

{- The Twilio monad -}

-- | This monad allows you to make authenticated REST API requests to Twilio
-- using your 'AccountSID' and 'AuthToken'.
type Twilio = TwilioT IO

-- | Run zero or more REST API requests to Twilio.
runTwilio :: Credentials -> Twilio a -> IO a
runTwilio = runTwilioT

{- | Parse an 'AccountSID' and 'AuthToken' before running zero or more REST API
requests to Twilio.

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
runTwilio' :: IO String  -- ^ Account SID
           -> IO String  -- ^ Authentication Token
           -> Twilio a
           -> IO a
runTwilio' = runTwilioT'

{- The Twilio monad transformer -}

-- | This monad transformer allows you to make authenticated REST API requests
-- to Twilio using your 'AccountSID' and 'AuthToken'.
newtype TwilioT m a = TwilioT (Monad m => (Credentials, AccountSID) -> RequestT m a)

getTwilioT :: Monad m => TwilioT m a -> (Credentials, AccountSID) -> RequestT m a
getTwilioT (TwilioT f) = f

instance Monad m => MonadRequest (TwilioT m) where
  request go r
    = TwilioT $ \config -> RequestT . FreeT . return . Free
    $ RequestF (r, \response -> runRequestT $ getTwilioT (go response) config)

-- | Run zero or more REST API requests to Twilio, unwrapping the inner monad
-- @m@.
runTwilioT :: MonadIO m => Credentials -> TwilioT m a -> m a
runTwilioT credentials@(accountSID, authToken) (TwilioT go) = do
  let basicAuthCredentials = (getSID accountSID, getAuthToken authToken)
  let requestM = go (credentials, accountSID)
  runRequest' basicAuthCredentials requestM

-- | Parse an 'AccountSID' and 'AuthToken' before running zero or more REST API
-- requests to Twilio, unwrapping the inner monad @m@.
runTwilioT' :: (MonadThrow m, MonadIO m)
            => m String     -- ^ Account SID
            -> m String     -- ^ Authentication Token
            -> TwilioT m a
            -> m a
runTwilioT' getAccountSID getAuthToken twilio = do
  accountSID <- T.pack <$> getAccountSID
  authToken  <- T.pack <$> getAuthToken
  case parseCredentials accountSID authToken of
    Nothing -> throwM InvalidCredentials
    Just credentials -> runTwilioT credentials twilio

instance Functor (TwilioT m) where
  fmap f ma = TwilioT $ \credentials -> do
    a <- getTwilioT ma credentials
    return $ f a

liftTwilioT :: m a -> TwilioT m a
liftTwilioT m = TwilioT (const (lift m))

instance Applicative m => Applicative (TwilioT m) where
  pure = liftTwilioT . pure
  f <*> v = TwilioT $ \r -> getTwilioT f r <*> getTwilioT v r

{-
instance Alternative m => Alternative (TwilioT m) where
  empty = liftTwilioT empty
  m <|> n = TwilioT $ \r -> getTwilioT m r <|> getTwilioT n r
-}

instance Monad m => Monad (TwilioT m) where
  return a = TwilioT (return . const a)
  m >>= k = TwilioT $ \client -> do
    a <- getTwilioT m client
    getTwilioT (k a) client

instance Monad m => MonadReader (Credentials, AccountSID) (TwilioT m) where
  ask = TwilioT return
  local f m = TwilioT $ getTwilioT m . f

instance MonadThrow m => MonadThrow (TwilioT m) where
  throwM = liftTwilioT . throwM

instance MonadTrans TwilioT where
  lift m = TwilioT $ const (lift m)

instance MonadIO m => MonadIO (TwilioT m) where
  liftIO = lift . liftIO

{- Types -}

-- | Your 'AccountSID' and 'AuthToken' are used to make authenticated REST API
-- requests to Twilio.
type Credentials = (AccountSID, AuthToken)

parseCredentials
  :: Text               -- ^ Account SID
  -> Text               -- ^ Authentication Token
  -> Maybe Credentials
parseCredentials accountSID authToken = uncurry (liftM2 (,))
  ( parseSID accountSID :: Maybe AccountSID
  , parseAuthToken authToken )

-- | The set of 'Exception's that may be thrown when attempting to make
-- requests against Twilio's REST API.
data TwilioException
  = InvalidSID         !Text
  | InvalidAuthToken   !Text
  | InvalidCredentials
  | UnexpectedResponse !(Response LBS.ByteString)
  deriving (Show, Typeable)

instance Exception TwilioException
