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

import Twilio.Types hiding (CallStatus(..), CallDirection(..))

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data Transcription = Transcription
  { sid                  :: !TranscriptionSID
  , dateCreated          :: !UTCTime
  , dateUpdated          :: !UTCTime
  , accountSID           :: !AccountSID
  , status               :: !TranscriptionStatus
  , recordingSID         :: !RecordingSID
  , duration             :: !(Maybe Int)
  , transcriptionText    :: !String
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
    <*> (v .: "duration"     <&> fmap safeRead
                             >>= maybeReturn')
    <*>  v .: "transcription_text"
    <*> (v .: "price"        <&> fmap safeRead
                             >>= maybeReturn')
    <*>  v .: "price_unit"
    <*>  v .: "api_version"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 TranscriptionSID Transcription where
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    ("/Transcriptions/" ++ sid ++ ".json")

-- | Get a 'Transcription' by 'TranscriptionSID'.
get :: (MonadThrow m, MonadIO m) => TranscriptionSID -> TwilioT m Transcription
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

