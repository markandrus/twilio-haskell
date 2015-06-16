{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverlappingInstances #-}
{-#LANGUAGE RankNTypes #-}

module Twilio.Internal.Resource
  ( -- $about
    Get(..)
  , Get0(..)
  , Get1(..)
  , Get2(..)
  , Post(..)
  , Post0(..)
  , Post1(..)
  , Post2(..)
  , Post3(..)
  , Delete(..)
  , Delete1(..)
  , parseJSONFromResponse
  ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client

import Control.Monad.Twilio

-- $about This module repackages functionality exposed by 'MonadRequest' into a
-- set of classes that REST resources can easily consume. It also provides
-- functions 'get' and 'post' that work well with type inference.

-- | 'Get0' represents REST resources that support HTTP GET requests with 0 arguments.
class Get0 r where
  get0 :: MonadThrow m => TwilioT m r

-- | 'Get1' represents REST resources that support HTTP GET requests with 1 argument.
class Get1 a r where
  get1 :: MonadThrow m => a -> TwilioT m r

-- | 'Get2' represents REST resources that support HTTP GET requests with 2 arguments.
class Get2 a b r where
  get2 :: MonadThrow m => a -> b -> TwilioT m r

-- | 'Get' represents REST resources that support HTTP GET requests with any number of arguments.
class Get r where
  get :: r

-- | Instances of 'Get0' are instances of 'Get'.
instance (MonadThrow m, Get0 r) => Get (TwilioT m r) where
  get = get0

-- | Instances of 'Get1' are instances of 'Get'.
instance (MonadThrow m, Get1 a r) => Get (a -> TwilioT m r) where
  get = get1

-- | Instances of 'Get2' are instances of 'Get'.
instance (MonadThrow m, Get2 a b r) => Get (a -> b -> TwilioT m r) where
  get = get2

-- | 'Post0' represents REST resources that support HTTP POST requests with 0 arguments.
class Post0 r where
  post0 :: MonadThrow m => TwilioT m r

-- | 'Post1' represents REST resources that support HTTP POST requests with 1 argument.
class Post1 a r where
  post1 :: MonadThrow m => a -> TwilioT m r

-- | 'Post2' represents REST resources that support HTTP POST requests with 2 arguments.
class Post2 a b r where
  post2 :: MonadThrow m => a -> b -> TwilioT m r

-- | 'Post3' represents REST resources that support HTTP POST requests with 3 arguments.
class Post3 a b c r where
  post3 :: MonadThrow m => a -> b -> c -> TwilioT m r

-- | 'Post' represents REST resources that support HTTP POST requests with any number of arguments.
class Post r where
  post :: r

-- | Instances of 'Post0' are instances of 'Post'.
instance (MonadThrow m, Post0 r) => Post (TwilioT m r) where
  post = post0

-- | Instances of 'Post1' are instances of 'Post'.
instance (MonadThrow m, Post1 a r) => Post (a -> TwilioT m r) where
  post = post1

instance (MonadThrow m, Post2 a b r) => Post (a -> b -> TwilioT m r) where
  post = post2

instance (MonadThrow m, Post3 a b c r) => Post (a -> b -> c -> TwilioT m r) where
  post = post3

-- | 'Delete1' represents REST resources that support HTTP POST requests with 1 argument.
class Delete1 a where
  delete1 :: MonadThrow m => a -> TwilioT m ()

-- | 'Delete' represents REST resources that support HTTP DELETE requests with any number of arguments.
class Delete r where
  delete :: r

-- | Instances of 'Delete1' are instances of 'Delete'.
instance (MonadThrow m, Delete1 a) => Delete (a -> TwilioT m ()) where
  delete = delete1

parseJSONFromResponse :: (FromJSON a, MonadThrow m) => Response LBS.ByteString -> m a
parseJSONFromResponse response =
  case eitherDecode (responseBody response) >>= parseEither parseJSON of
    Left  _ -> throwM $ UnexpectedResponse response
    Right a -> return a
