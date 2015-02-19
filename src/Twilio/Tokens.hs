{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Tokens
  ( -- * Resource
    Token(..)
  , IceServer(..)
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join, mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack, unpack)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

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
    <*> (v .: "ttl"          >>= safeRead)
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
