{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE StandaloneDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.SID
-- Copyright   :  (C) 2016- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module defines all of the SIDs (string identifiers) for Twilio resources
-- in a single place.
-------------------------------------------------------------------------------
module Twilio.Types.SID where

import Control.DeepSeq (NFData)
import Control.Monad (MonadPlus, mzero)
import Data.Aeson
import Data.Binary (Binary)
import Data.Bits (countLeadingZeros)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Read (readPrec)
import Numeric (readHex, showHex)
import Text.ParserCombinators.ReadP (char, count, get, skipSpaces)
import Text.Read (ReadPrec, parens, readP_to_Prec, readPrec_to_S)

import Twilio.Types.Alpha

-- SID
-------------------------------------------------------------------------------

-- | A SID (string identifier) is a 34-character string. The first two
-- characters are capital letters A through Z; the remaining 32 characters
-- represent a 128-bit natural number in hexadecimal.
data SID (a :: Alpha) (b :: Alpha) = SID !Word64 !Word64
  deriving (Bounded, Data, Eq, Generic, Ix, Ord, Typeable)

class IsSID sid where
  getSID :: sid -> Text
  parseSID :: Text -> Maybe sid

instance (IsAlpha a, IsAlpha b) => IsSID (SID a b) where
  getSID = sidToText
  parseSID = parseSIDFromText

instance Binary (SID a b)

instance Hashable (SID a b)

instance NFData (SID a b)

instance (IsAlpha a, IsAlpha b) => IsString (SID a b) where
  fromString = read

instance (IsAlpha a, IsAlpha b) => Read (SID a b) where
  readPrec = readSID

readSID :: forall a b. (IsAlpha a, IsAlpha b) => ReadPrec (SID a b)
readSID = parens . readP_to_Prec . const $ do
    skipSpaces
    char $ salphaToChar sa
    char $ salphaToChar sb
    chars <- count 16 get
    case readHex chars of
      [(word1, _)] -> do
        chars <- count 16 get
        case readHex chars of
          [(word2, _)] -> pure $ SID word1 word2
          _            -> mzero
      _            -> mzero
  where
    sa :: SAlpha a
    sa = promote :: SAlpha a

    sb :: SAlpha b
    sb = promote :: SAlpha b

instance (IsAlpha a, IsAlpha b) => Show (SID a b) where
  show (SID word1 word2) = show (demote (promote :: SAlpha a))
                        <> show (demote (promote :: SAlpha b))
                        <> showHex64 word1
                        <> showHex64 word2
    where
      showHex64 :: Word64 -> String
      showHex64 word64 = replicate padding '0' <> showHex word64 ""
        where
          padding = (countLeadingZeros word64 - 1) `quot` 4

parseSIDFromText :: forall m a b. (MonadPlus m, IsAlpha a, IsAlpha b) => Text -> m (SID a b)
parseSIDFromText text = case readPrec_to_S readSID 0 $ T.unpack (T.take 32 text) of
  [(sid, [])] -> pure sid
  _ -> mzero

parseSIDFromJSON :: (MonadPlus m, IsAlpha a, IsAlpha b) => Value -> m (SID a b)
parseSIDFromJSON (String text) = parseSIDFromText text
parseSIDFromJSON _ = mzero

sidToJSON :: (IsAlpha a, IsAlpha b) => SID a b -> Value
sidToJSON = String . sidToText

sidToText :: (IsAlpha a, IsAlpha b) => SID a b -> Text
sidToText = T.pack . show

instance (IsAlpha a, IsAlpha b) => FromJSON (SID a b) where
  parseJSON = parseSIDFromJSON

instance (IsAlpha a, IsAlpha b) => ToJSON (SID a b) where
  toJSON = sidToJSON
