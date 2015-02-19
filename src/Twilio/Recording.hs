{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Recording
  ( -- * Resource
    Recording(..)
  , get
  , get'
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

{- Resource -}
data Recording = Recording
  { sid                  :: !RecordingSID
  , dateCreated          :: !UTCTime
  , dateUpdated          :: !UTCTime
  , accountSID           :: !AccountSID
  , callSID         		 :: !CallSID
  , duration             :: !(Maybe Int)
  , apiVersion           :: !APIVersion
  , uri                  :: !URI
  } deriving (Show, Eq)


instance FromJSON Recording where
  parseJSON (Object v) = Recording
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "call_sid"
    <*> (v .: "duration"     <&> fmap safeRead
                             >>= maybeReturn')
    <*>  v .: "api_version"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

-- | Get a 'Recording' by 'RecordingSID'.
get :: (MonadThrow m, MonadIO m) => RecordingSID -> TwilioT m Recording
get recordingSID
  = requestForAccount $ "/Recordings/" ++ getSID recordingSID ++ ".json"

-- | Get an account's 'Recording' by 'RecordingSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> RecordingSID
     -> TwilioT m Recording
get' accountSID recordingSID = forAccount accountSID $ get recordingSID
