{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Tokens
  ( -- * Resource
    Token(..)
  , IceServer(..)
  , Twilio.Tokens.post
  ) where

import Control.Applicative
import Control.Error.Safe
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Network.URI

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
    let url = HashMap.lookup "url" map >>= valueToText >>= parseAbsoluteURI . T.unpack
    in  case url of
      Nothing   -> mzero
      Just url' -> return . fromMaybe (StunServer url') $ TurnServer
        <$> url
        <*> (HashMap.lookup "credential" map >>= valueToText)
        <*> (HashMap.lookup "username"   map >>= valueToText)
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
