{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Types.Issue
  ( Issue(..)
  ) where

import Control.Monad
import Data.Aeson
import Data.Data
import GHC.Generics

-- | A list of issues experienced during the call.
data Issue
  = ImperfectAudio     -- ^ Imperfect audio quality: Choppy, echoed, or garbled audio during conversation.
  | DroppedCall        -- ^ Dropped call: call initially connected but was dropped.
  | IncorrectCallerId  -- ^ Incorrect caller ID: Call connected but caller ID displayed \'Unknown\' or an incorrect number.
  | PostDialDelay      -- ^ Post dial delay: Call connected but there was a long delay between dialing the phone number and the start of ringing.
  | DigitsNotCaptured  -- ^ DTMF tones not captured: Failed to capture digit input on phone menus.
  | UnsolicitedCall    -- ^ Unsolicited call: Received telemarketer, wrong number, automated or other type of unsolicited call.
  | AudioLatency       -- ^ Audio latency: Call participants can hear each other but with significant audio delay.
  | OneWayAudio        -- ^ One way audio: Only one party could hear the audio during the conversation.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON Issue where
  parseJSON (String "imperfect-audio")     = return ImperfectAudio
  parseJSON (String "dropped-call")        = return DroppedCall
  parseJSON (String "incorrect-caller-id") = return IncorrectCallerId
  parseJSON (String "post-dial-delay")     = return PostDialDelay
  parseJSON (String "digits-not-captured") = return DigitsNotCaptured
  parseJSON (String "unsolicited-call")    = return UnsolicitedCall
  parseJSON (String "audio-latency")       = return AudioLatency
  parseJSON (String "one-way-audio")       = return OneWayAudio
  parseJSON _ = mzero

instance ToJSON Issue where
  toJSON ImperfectAudio    = String "imperfect-audio"
  toJSON DroppedCall       = String "dropped-call"
  toJSON IncorrectCallerId = String "incorrect-caller-id"
  toJSON PostDialDelay     = String "post-dial-delay"
  toJSON DigitsNotCaptured = String "digits-not-captured"
  toJSON UnsolicitedCall   = String "unsolicited-call"
  toJSON AudioLatency      = String "audio-latency"
  toJSON OneWayAudio       = String "one-way-audio"
