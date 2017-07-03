{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.SIDs
-- Copyright   :  (C) 2016- Mark Andrus Roberts
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
  , PhoneNumberSID(..)
  , QueueSID(..)
  , RecordingSID(..)
  , ShortCodeSID(..)
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
  , mkMessageSID
  , mkPhoneNumberSID
  , mkQueueSID
  , mkRecordingSID
  , mkShortCodeSID
  , mkTranscriptionSID
  , mkUsageTriggerSID
  ) where

import Control.DeepSeq (NFData)
import Data.String (IsString(fromString))
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Twilio.Types.Alpha (Alpha(..))
import Twilio.Types.SID2 (SID(..))
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
createSID S M "Message"
createSID P N "PhoneNumber"
createSID Q U "Queue"
createSID R E "Recording"
createSID S C "ShortCode"
createSID T R "Transcription"
createSID U T "UsageTrigger"
