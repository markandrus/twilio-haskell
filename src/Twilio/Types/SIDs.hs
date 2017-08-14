{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.SIDs
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module defines all of the SIDs (string identifiers) for Twilio resources
-- in a single place.
-------------------------------------------------------------------------------
module Twilio.Types.SIDs
  ( AccountSID(..)
  , AddressSID(..)
  , APIKeySID(..)
  , ApplicationSID(..)
  , CallSID(..)
  , ConferenceSID(..)
  , ConnectAppSID(..)
  , CredentialSID(..)
  , CredentialListSID(..)
  , DomainSID(..)
  , FeedbackSummarySID(..)
  , IPAccessControlListSID(..)
  , IPAddressSID(..)
  , MediaSID(..)
  , MessageSID(..)
  , MMSMessageSID(..)
  , PhoneNumberSID(..)
  , QueueSID(..)
  , RecordingSID(..)
  , ShortCodeSID(..)
  , SMSMessageSID(..)
  , TranscriptionSID(..)
  , UsageTriggerSID(..)
    -- * Smart Constructors
  , mkAccountSID
  , mkAddressSID
  , mkAPIKeySID
  , mkApplicationSID
  , mkCallSID
  , mkConferenceSID
  , mkConnectAppSID
  , mkCredentialSID
  , mkCredentialListSID
  , mkDomainSID
  , mkFeedbackSummarySID
  , mkIPAccessControlListSID
  , mkIPAddressSID
  , mkMediaSID
  , mkMMSMessageSID
  , mkPhoneNumberSID
  , mkQueueSID
  , mkRecordingSID
  , mkShortCodeSID
  , mkSMSMessageSID
  , mkTranscriptionSID
  , mkUsageTriggerSID
  , module Twilio.Types.SID
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.String (IsString)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Twilio.Types.Alpha (Alpha(..))
import Twilio.Types.SID (IsSID(..), SID(..))
import Twilio.Types.SID.TH (createSID)

-- SIDs
-------------------------------------------------------------------------------

createSID A C "Account"
createSID A D "Address"
createSID S K "APIKey"
createSID A P "Application"
createSID C A "Call"
createSID C O "Conference"
createSID C N "ConnectApp"
createSID S C "Credential"
createSID C L "CredentialList"
createSID S D "Domain"
createSID F S "FeedbackSummary"
createSID A L "IPAccessControlList"
createSID I P "IPAddress"
createSID M E "Media"
createSID M M "MMSMessage"
createSID P N "PhoneNumber"
createSID Q U "Queue"
createSID R E "Recording"
createSID S C "ShortCode"
createSID S M "SMSMessage"
createSID T R "Transcription"
createSID U T "UsageTrigger"

newtype MessageSID = MessageSID {
  getMessageSID :: Either SMSMessageSID MMSMessageSID
} deriving (Data, Eq, Generic, Hashable, NFData, Ord, Read, Show, Typeable)

instance IsSID MessageSID where
  getSID = (either getSID getSID) . getMessageSID
  parseSID text =  MessageSID
               <$> ((Left  <$> parseSID text)
               <|>  (Right <$> parseSID text))

instance ToJSON MessageSID where
  toJSON = (either toJSON toJSON) . getMessageSID

instance FromJSON MessageSID where
  parseJSON value =  MessageSID
                 <$> ((Left  <$> parseJSON value)
                 <|>  (Right <$> parseJSON value))
