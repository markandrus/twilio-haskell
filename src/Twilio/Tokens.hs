{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Tokens
  ( -- * Resource
    Token(..)
  , IceServer(..)
  ) where

import Control.Applicative
import Control.Error.Safe
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text
import Data.Time.Clock
import Network.URI

import Twilio.Types
import Twilio.Internal.Parser

{- Resource -}

data Token = Token
  { accountSID  :: !AccountSID
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  , iceServers  :: [IceServer]
  , password    :: !String
  , ttl         :: !Integer
  , username    :: !String
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
               , turnCredential :: !String
               , turnUsername   :: !String }
  deriving (Eq, Show)

instance FromJSON IceServer where
  parseJSON (Object map) =
    let url = HashMap.lookup "url" map >>= valueToString >>= parseRelativeReference
    in  case url of
      Nothing   -> mzero
      Just url' -> return . fromMaybe (StunServer url') $ TurnServer
        <$> url
        <*> (HashMap.lookup "credential" map >>= valueToString)
        <*> (HashMap.lookup "username"   map >>= valueToString)
  parseJSON _ = mzero

valueToString :: Value -> Maybe String
valueToString (String v) = Just $ unpack v
valueToString _ = Nothing
