{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Transcription
  ( -- * Resource
    Transcription(..)
  , Twilio.Transcription.get
    -- * Types
  , PriceUnit(..)
  , TranscriptionStatus(..)
  ) where

import Control.Applicative
import Control.Error.Safe
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Network.URI

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Transcription = Transcription
  { sid                  :: !TranscriptionSID
  , dateCreated          :: !UTCTime
  , dateUpdated          :: !UTCTime
  , accountSID           :: !AccountSID
  , status               :: !TranscriptionStatus
  , recordingSID         :: !RecordingSID
  , duration             :: !(Maybe Int)
  , transcriptionText    :: !Text
  , price                :: !(Maybe Double)
  , priceUnit            :: !PriceUnit
  , apiVersion           :: !APIVersion
  , uri                  :: !URI
  } deriving (Show, Eq)

instance FromJSON Transcription where
  parseJSON (Object v) = Transcription
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "status"
    <*>  v .: "recording_sid"
    <*> (v .: "duration"     <&> fmap readZ
                             >>= maybeReturn')
    <*>  v .: "transcription_text"
    <*> (v .: "price"        <&> fmap readZ
                             >>= maybeReturn')
    <*>  v .: "price_unit"
    <*>  v .: "api_version"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 TranscriptionSID Transcription where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Transcriptions/" <> sid <> ".json")

-- | Get a 'Transcription' by 'TranscriptionSID'.
get :: MonadThrow m => TranscriptionSID -> TwilioT m Transcription
get = Resource.get

{- Types -}

data TranscriptionStatus
  = InProgress
  | Completed
  | Failed
  deriving Eq

instance Show TranscriptionStatus where
  show InProgress  = "in-progress"
  show Completed   = "completed"
  show Failed      = "failed"

instance FromJSON TranscriptionStatus where
  parseJSON (String "in-progress")   = return InProgress
  parseJSON (String "completed")     = return Completed
  parseJSON (String "failed")        = return Failed
  parseJSON _ = mzero
