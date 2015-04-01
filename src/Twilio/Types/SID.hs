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
  , MessageSID
  , PhoneNumberSID
  , RecordingSID
  , TranscriptionSID
  , UsageTriggerSID
  ) where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Bifunctor.Flip
import Data.Typeable
import Data.Text
import GHC.Generics

newtype AccountSID = AccountSID { getAccountSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype AddressSID = AddressSID { getAddressSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype ApplicationSID = ApplicationSID { getApplicationSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype CallSID = CallSID { getCallSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype ConnectAppSID = ConnectAppSID { getConnectAppSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype MessageSID = MessageSID { getMessageSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype PhoneNumberSID = PhoneNumberSID { getPhoneNumberSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype RecordingSID = RecordingSID { getRecordingSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype TranscriptionSID = TranscriptionSID { getTranscriptionSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

newtype UsageTriggerSID = UsageTriggerSID { getUsageTriggerSID :: String }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance SID AccountSID where
  getPrefix = Const ('A', 'C')

instance FromJSON AccountSID where
  parseJSON = parseSIDFromJSON

instance ToJSON AccountSID where
  toJSON = sidToJSON

instance SID AddressSID where
  getPrefix = Const ('A', 'D')

instance FromJSON AddressSID where
  parseJSON = parseSIDFromJSON

instance ToJSON AddressSID where
  toJSON = sidToJSON

instance SID ApplicationSID where
  getPrefix = Const ('A', 'P')

instance FromJSON ApplicationSID where
  parseJSON = parseSIDFromJSON

instance ToJSON ApplicationSID where
  toJSON = sidToJSON

instance SID CallSID where
  getPrefix = Const ('C', 'A')

instance FromJSON CallSID where
  parseJSON = parseSIDFromJSON

instance ToJSON CallSID where
  toJSON = sidToJSON

instance SID ConnectAppSID where
  getPrefix = Const ('C', 'N')

instance FromJSON ConnectAppSID where
  parseJSON = parseSIDFromJSON

instance ToJSON ConnectAppSID where
  toJSON = sidToJSON

instance SID MessageSID where
  getPrefix = Const ('S', 'M')

instance FromJSON MessageSID where
  parseJSON = parseSIDFromJSON

instance ToJSON MessageSID where
  toJSON = sidToJSON

instance SID PhoneNumberSID where
  getPrefix = Const ('P', 'N')

instance FromJSON PhoneNumberSID where
  parseJSON = parseSIDFromJSON

instance ToJSON PhoneNumberSID where
  toJSON = sidToJSON

instance SID RecordingSID where
  getPrefix = Const ('R', 'E')

instance FromJSON RecordingSID where
  parseJSON = parseSIDFromJSON

instance ToJSON RecordingSID where
  toJSON = sidToJSON

instance SID TranscriptionSID where
  getPrefix = Const ('T', 'R')

instance FromJSON TranscriptionSID where
  parseJSON = parseSIDFromJSON

instance ToJSON TranscriptionSID where
  toJSON = sidToJSON

instance SID UsageTriggerSID where
  getPrefix = Const ('U', 'T')

instance FromJSON UsageTriggerSID where
  parseJSON = parseSIDFromJSON

instance ToJSON UsageTriggerSID where
  toJSON = sidToJSON

parseSID' :: (MonadPlus m, SID s) => String -> Const (m s) s
parseSID' sid@(a:b:_)
  = runFlip $ (\ab' -> if (a, b) == ab' then return (makeSID sid) else mzero)
           <$> Flip getPrefix
parseSID' _ = Const mzero

parseSIDFromJSON :: (MonadPlus m, SID s) => Value -> m s
parseSIDFromJSON (String v) = getConst . parseSID' $ unpack v
parseSIDFromJSON _ = mzero

sidToJSON :: SID s => s -> Value
sidToJSON = String . pack . getSID

class SID s where
  getPrefix :: Const (Char, Char) s

  getSID :: s -> String
  default getSID :: (Generic s, GSID (Rep s ())) => s -> String
  getSID = (gGetSID :: Rep s () -> String) . from

  makeSID :: String -> s
  default makeSID :: (Generic s, GSID (Rep s ())) => String -> s
  makeSID = to . (gMakeSID :: String -> Rep s ())

  parseSID :: String -> Maybe s
  parseSID = getConst . parseSID'

class GSID s where
  gGetSID :: s -> String
  gMakeSID :: String -> s

instance GSID (D1 a (C1 b (S1 c (Rec0 String))) ()) where
  gGetSID (M1 (M1 (M1 (K1 s)))) = s
  gMakeSID = M1 . M1 . M1 . K1
