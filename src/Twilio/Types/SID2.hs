{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE StandaloneDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.SID2
-- Copyright   :  (C) 2016- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module defines all of the SIDs (string identifiers) for Twilio resources
-- in a single place.
-------------------------------------------------------------------------------
module Twilio.Types.SID2
  -- ( SID(..)
  -- ) where
  where

import Control.DeepSeq (NFData)
import Control.Monad (mzero)
import Data.Binary (Binary)
import Data.Bits (countLeadingZeros)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Read (readPrec)
import Numeric (readHex, showHex)
import Text.ParserCombinators.ReadP (char, count, get, skipSpaces)
import Text.Read (ReadPrec, parens, readP_to_Prec)

import Twilio.Types.Alpha (Alpha(..))
import Twilio.Types.Sing (Sing, SingI(..), SingKind(..))

-- SID
-------------------------------------------------------------------------------

-- | A SID (string identifier) is a 34-character string. The first two
-- characters are capital letters A through Z; the remaining 32 characters
-- represent a 128-bit natural number in hexadecimal.
data SID (a :: Alpha) (b :: Alpha) = SID !Word64 !Word64
  deriving (Bounded, Data, Eq, Generic, Ix, Ord, Typeable)

instance Binary (SID a b)

instance Hashable (SID a b)

instance NFData (SID a b)

instance (SingI a, SingI b) => IsString (SID a b) where
  fromString = read

instance (SingI a, SingI b) => Read (SID a b) where
  readPrec = readSID

readSID :: forall a b. (SingI a, SingI b) => ReadPrec (SID a b)
readSID = parens . readP_to_Prec . const $ do
    skipSpaces
    char . head . show $ fromSing sa
    char . head . show $ fromSing sb
    chars <- count 16 get
    case readHex chars of
      [(word1, _)] -> do
        chars <- count 16 get
        case readHex chars of
          [(word2, _)] -> pure $ SID word1 word2
          _            -> mzero
      _            -> mzero
  where
    sa :: Sing a
    sa = sing :: Sing a

    sb :: Sing b
    sb = sing :: Sing b

instance (SingI a, SingI b) => Show (SID a b) where
  show (SID word1 word2) = show (fromSing (sing :: Sing a))
                        <> show (fromSing (sing :: Sing b))
                        <> showHex64 word1
                        <> showHex64 word2
    where
      showHex64 :: Word64 -> String
      showHex64 word64 = replicate padding '0' <> showHex word64 ""
        where
          padding = (countLeadingZeros word64 - 1) `quot` 4
