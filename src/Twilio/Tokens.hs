{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Tokens
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Tokens
  ( -- * Resource
    Token(..)
  , IceServer(..)
  , Twilio.Tokens.post
  ) where

import Control.Error.Safe
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Network.URI

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as KeyMap
#endif

import Control.Monad.Twilio
import Twilio.Types
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data Token = Token
  { accountSID  :: !AccountSID
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  , iceServers  :: [IceServer]
  , password    :: !Text
  , ttl         :: !Integer
  , username    :: !Text
  } deriving (Eq, Show)

instance FromJSON Token where
  parseJSON (Object v) = Token
    <$>  v .: "account_sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "ice_servers"
    <*>  v .: "password"
    <*> (v .: "ttl"          >>= readZ)
    <*>  v .: "username"
  parseJSON _ = mzero

data IceServer
  = StunServer { stunURL        :: !URI }
  | TurnServer { turnURL        :: !URI
               , turnCredential :: !Text
               , turnUsername   :: !Text }
  deriving (Eq, Show)

instance FromJSON IceServer where
  parseJSON (Object map) =
    let url = KeyMap.lookup "url" map >>= valueToText >>= parseAbsoluteURI . T.unpack
    in  case url of
      Nothing   -> mzero
      Just url' -> return . fromMaybe (StunServer url') $ TurnServer
        <$> url
        <*> (KeyMap.lookup "credential" map >>= valueToText)
        <*> (KeyMap.lookup "username"   map >>= valueToText)
  parseJSON _ = mzero

instance Post0 Token where
  post0 = request parseJSONFromResponse =<<
    makeTwilioPOSTRequest "/Tokens.json" []

instance Post1 Integer Token where
  post1 (show -> ttl) = request parseJSONFromResponse =<<
    makeTwilioPOSTRequest "/Tokens.json"
      [ ("Ttl", encodeUtf8 . T.pack $ ttl ) ]

instance Post1 (Maybe Integer) Token where
  post1 Nothing = post0
  post1 (Just ttl) = post1 ttl

post :: MonadThrow m => Maybe Integer -> TwilioT m Token
post = Resource.post
