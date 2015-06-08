{-#OPTIONS_GHC -fno-warn-unused-binds #-}
{-#LANGUAGE DefaultSignatures #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}

module Twilio.Types.SID
  ( -- * String Identifier (SID)
    SID(getSID, parseSID)
    -- ** Instances
  , AccountSID
  , AddressSID
  , ApplicationSID
  , CallSID
  , ConnectAppSID
  , FeedbackSummarySID
  , MessageSID
  , PhoneNumberSID
  , RecordingSID
  , TranscriptionSID
  , UsageTriggerSID
  ) where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Data
import Data.Bifunctor.Flip
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

{- Account SID -}

newtype AccountSID = AccountSID { getAccountSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID AccountSID where
  getPrefix = Const ('A', 'C')

instance FromJSON AccountSID where
  parseJSON = parseSIDFromJSON

instance ToJSON AccountSID where
  toJSON = sidToJSON

{- Address SID -}

newtype AddressSID = AddressSID { getAddressSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID AddressSID where
  getPrefix = Const ('A', 'D')

instance FromJSON AddressSID where
  parseJSON = parseSIDFromJSON

instance ToJSON AddressSID where
  toJSON = sidToJSON

{- Application SID -}

newtype ApplicationSID = ApplicationSID { getApplicationSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID ApplicationSID where
  getPrefix = Const ('A', 'P')

instance FromJSON ApplicationSID where
  parseJSON = parseSIDFromJSON

instance ToJSON ApplicationSID where
  toJSON = sidToJSON

{- Call SID -}

newtype CallSID = CallSID { getCallSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID CallSID where
  getPrefix = Const ('C', 'A')

instance FromJSON CallSID where
  parseJSON = parseSIDFromJSON

instance ToJSON CallSID where
  toJSON = sidToJSON

{- Connect App SID -}

newtype ConnectAppSID = ConnectAppSID { getConnectAppSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID ConnectAppSID where
  getPrefix = Const ('C', 'N')

instance FromJSON ConnectAppSID where
  parseJSON = parseSIDFromJSON

instance ToJSON ConnectAppSID where
  toJSON = sidToJSON

{- Feedback Summary SID -}

newtype FeedbackSummarySID = FeedbackSummarySID { getFeedbackSummarySID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID FeedbackSummarySID where
  getPrefix = Const ('F', 'S')

instance FromJSON FeedbackSummarySID where
  parseJSON = parseSIDFromJSON

instance ToJSON FeedbackSummarySID where
  toJSON = sidToJSON

{- Message SID -}

newtype MessageSID = MessageSID { getMessageSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID MessageSID where
  getPrefix = Const ('S', 'M')

instance FromJSON MessageSID where
  parseJSON = parseSIDFromJSON

instance ToJSON MessageSID where
  toJSON = sidToJSON

{- Phone Number SID -}

newtype PhoneNumberSID = PhoneNumberSID { getPhoneNumberSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID PhoneNumberSID where
  getPrefix = Const ('P', 'N')

instance FromJSON PhoneNumberSID where
  parseJSON = parseSIDFromJSON

instance ToJSON PhoneNumberSID where
  toJSON = sidToJSON

{- Recording SID -}

newtype RecordingSID = RecordingSID { getRecordingSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID RecordingSID where
  getPrefix = Const ('R', 'E')

instance FromJSON RecordingSID where
  parseJSON = parseSIDFromJSON

instance ToJSON RecordingSID where
  toJSON = sidToJSON

{- Transcription SID -}

newtype TranscriptionSID = TranscriptionSID { getTranscriptionSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID TranscriptionSID where
  getPrefix = Const ('T', 'R')

instance FromJSON TranscriptionSID where
  parseJSON = parseSIDFromJSON

instance ToJSON TranscriptionSID where
  toJSON = sidToJSON

{- Usage Trigger SID -}

newtype UsageTriggerSID = UsageTriggerSID { getUsageTriggerSID :: Text }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance SID UsageTriggerSID where
  getPrefix = Const ('U', 'T')

instance FromJSON UsageTriggerSID where
  parseJSON = parseSIDFromJSON

instance ToJSON UsageTriggerSID where
  toJSON = sidToJSON

parseSID' :: (MonadPlus m, SID s) => Text -> Const (m s) s
parseSID' sid =
  case T.unpack sid of
    a:b:_ -> runFlip $ (\ab' -> if (a, b) == ab' then return (makeSID sid) else mzero)
               <$> Flip getPrefix
    _     -> Const mzero

parseSIDFromJSON :: (MonadPlus m, SID s) => Value -> m s
parseSIDFromJSON (String v) = getConst $ parseSID' v
parseSIDFromJSON _ = mzero

sidToJSON :: SID s => s -> Value
sidToJSON = String . getSID

class SID s where
  getPrefix :: Const (Char, Char) s

  getSID :: s -> Text
  default getSID :: (Generic s, GSID (Rep s ())) => s -> Text
  getSID = (gGetSID :: Rep s () -> Text) . from

  makeSID :: Text -> s
  default makeSID :: (Generic s, GSID (Rep s ())) => Text -> s
  makeSID = to . (gMakeSID :: Text -> Rep s ())

  parseSID :: Text -> Maybe s
  parseSID = getConst . parseSID'

class GSID s where
  gGetSID :: s -> Text
  gMakeSID :: Text -> s

instance GSID (D1 a (C1 b (S1 c (Rec0 Text))) ()) where
  gGetSID (M1 (M1 (M1 (K1 s)))) = s
  gMakeSID = M1 . M1 . M1 . K1
