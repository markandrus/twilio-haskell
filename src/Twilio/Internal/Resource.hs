{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverlappingInstances #-}
{-#LANGUAGE RankNTypes #-}

module Twilio.Internal.Resource
  ( -- $about
    Get(..)
  , Get0(..)
  , Get1(..)
  , Post(..)
  , Post0(..)
  , Post1(..)
  , Post2(..)
  , parseJSONFromResponse
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client

import Twilio.Types (TwilioT)
import Twilio.Internal.Request (MonadRequest)

-- $about This module repackages functionality exposed by 'MonadRequest' into a
-- set of classes that REST resources can easily consume. It also provides
-- functions 'get' and 'post' that work well with type inference.

-- | 'Get0' represents REST resources that support HTTP GET requests with 0 arguments.
class Get0 r where
  get0 :: Monad m => TwilioT m r

-- | 'Get1' represents REST resources that support HTTP GET requests with 1 argument.
class Get1 a r where
  get1 :: Monad m => a -> TwilioT m r

-- | 'Get' represents REST resources that support HTTP GET requests with any number of arguments.
class Get r where
  get :: r

-- | Instances of 'Get0' are instances of 'Get'.
instance (Monad m, Get0 r) => Get (TwilioT m r) where
  get = get0

-- | Instances of 'Get1' are instances of 'Get'.
instance (Monad m, Get1 a r) => Get (a -> TwilioT m r) where
  get = get1

-- | 'Post0' represents REST resources that support HTTP POST requests with 0 arguments.
class Post0 r where
  post0 :: Monad m => TwilioT m r

-- | 'Post1' represents REST resources that support HTTP POST requests with 1 argument.
class Post1 a r where
  post1 :: Monad m => a -> TwilioT m r

-- | 'Post1' represents REST resources that support HTTP POST requests with 2 arguments.
class Post2 a b r where
  post2 :: Monad m => a -> b -> TwilioT m r

-- | 'Post' represents REST resources that support HTTP POST requests with any number of arguments.
class Post r where
  post :: r

-- | Instances of 'Post0' are instances of 'Post'.
instance (Monad m, Post0 r) => Post (TwilioT m r) where
  post = post0

-- | Instances of 'Post1' are instances of 'Post'.
instance (Monad m, Post1 a r) => Post (a -> TwilioT m r) where
  post = post1

instance (Monad m, Post2 a b r) => Post (a -> b -> TwilioT m r) where
  post = post2

parseJSONFromResponse :: FromJSON a => Response LBS.ByteString -> Maybe a
parseJSONFromResponse response =
  decode (responseBody response) >>= parseMaybe parseJSON
