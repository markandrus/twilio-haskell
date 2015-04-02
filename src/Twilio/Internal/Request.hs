{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}

module Twilio.Internal.Request where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude hiding (head)

-- | 'RequestF' represents an HTTP request and stores a continuaton for the
-- eventual 'Response' to the request.
newtype RequestF a = RequestF (Request, Response LBS.ByteString -> a)
  deriving (Functor, Generic, Typeable)

-- | @'RequestT' m a@ augments an existing monad @m@ with the ability to
-- perform HTTP requests for 'Resource's.
newtype RequestT m a = RequestT { runRequestT :: FreeT RequestF m a }
  deriving (Applicative, Functor, Generic, Monad, MonadIO, MonadTrans, Typeable)

class Monad m => MonadRequest m where
  request :: (Response LBS.ByteString -> m a) -> Request -> m a

instance Monad m => MonadRequest (RequestT m) where
  request go r = RequestT . FreeT . return . Free $ RequestF (r, runRequestT . go)

-- | A dummy interpreter
{-
runRequest :: MonadIO m => RequestT m a -> m a
runRequest (RequestT (FreeT m)) = m >>= \case
    Free f -> runRequest . RequestT $ run f
    Pure a -> return a
  where
    run (RequestF (_, go)) = undefined
-}

baseURL :: String
baseURL = "https://api.twilio.com/2010-04-01"

runRequest' :: (Monad m, MonadIO m) => (String, String) -> RequestT m a -> m a
runRequest' credentials (RequestT (FreeT m)) = m >>= \case
    Free f -> runRequest' credentials . RequestT =<< run (return <$> f)
    Pure a -> return a
  where
    run (RequestF (request, go)) = do
      manager <- liftIO (newManager tlsManagerSettings)
      liftIO $ withResponse request manager $ \response -> do
        let body = responseBody response
        body' <- LBS.fromChunks <$> brConsume body
        go $ const body' <$> response
