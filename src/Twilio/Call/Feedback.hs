{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Call.Feedback
  ( -- * Resource
    Feedback(..)
  , Twilio.Call.Feedback.get
    -- * Types
  , Quality(..)
  , Issue(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Data
import Data.Monoid
import Data.Scientific
import Data.Time.Clock
import GHC.Generics

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

-- | 'Feedback' is a subresource of a 'Call' instance resource. It represents a call quality feedback entry for a given phone call.
data Feedback = Feedback
  { sid          :: !CallSID
  , accountSID   :: !AccountSID
  , qualityScore :: !Quality
  , issues       :: ![Issue]
  , dateCreated  :: !UTCTime
  , dateUpdated  :: !UTCTime
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON Feedback where
  parseJSON (Object v) = Feedback
    <$>  v .: "sid"
    <*>  v .: "account_sid"
    <*>  v .: "quality_score"
    <*>  v .: "issues"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
  parseJSON _ = mzero

instance Get1 CallSID Feedback where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Calls/" <> sid <> "/Feedback.json")

-- | Get a 'Call''s 'Feedback' by 'CallSID'.
get :: MonadThrow m => CallSID -> TwilioT m Feedback
get = Resource.get

{- Types -}

-- | An integer 1 to 5 quality score where 1 represents very poor call quality and 5 represents a perfect call.
data Quality
  = Q1  -- ^ Very poor call quality
  | Q2
  | Q3
  | Q4
  | Q5  -- ^ A perfect call
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance ToJSON Quality where
  toJSON Q1 = Number $ fromRational 1
  toJSON Q2 = Number $ fromRational 2
  toJSON Q3 = Number $ fromRational 3
  toJSON Q4 = Number $ fromRational 4
  toJSON Q5 = Number $ fromRational 5

instance FromJSON Quality where
  parseJSON (numberToMaybeInt -> Just 1) = return Q1
  parseJSON (numberToMaybeInt -> Just 2) = return Q2
  parseJSON (numberToMaybeInt -> Just 3) = return Q3
  parseJSON (numberToMaybeInt -> Just 4) = return Q4
  parseJSON (numberToMaybeInt -> Just 5) = return Q5
  parseJSON _ = mzero

numberToMaybeInt :: Value -> Maybe Int
numberToMaybeInt (Number n) = toBoundedInteger n
numberToMaybeInt _ = mzero
