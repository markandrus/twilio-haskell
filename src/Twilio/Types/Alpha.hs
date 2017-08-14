{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE TypeFamilies #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.Alpha
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module defines 'Alpha', a data type for the characters A through Z,
-- along with a singleton type.
-------------------------------------------------------------------------------
module Twilio.Types.Alpha
  ( Alpha(..)
  , alphaToChar
  , salphaToChar
  , SAlpha(..)
  , IsAlpha(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import GHC.Generics (Generic)

-- Alpha
-------------------------------------------------------------------------------

-- | Characters A through Z
data Alpha = A | B | C | D | E | F | G | H | I | J | K | L | M
           | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Binary, Bounded, Data, Enum, Eq, Generic, Ix, Ord, Read,
            Show, Typeable)

instance Hashable Alpha

instance NFData Alpha

alphaToChar :: Alpha -> Char
alphaToChar = head . show

salphaToChar :: IsAlpha a => SAlpha a -> Char
salphaToChar = alphaToChar . demote

data SAlpha (a :: Alpha) where
  SA :: SAlpha A
  SB :: SAlpha B
  SC :: SAlpha C
  SD :: SAlpha D
  SE :: SAlpha E
  SF :: SAlpha F
  SG :: SAlpha G
  SH :: SAlpha H
  SI :: SAlpha I
  SJ :: SAlpha J
  SK :: SAlpha K
  SL :: SAlpha L
  SM :: SAlpha M
  SN :: SAlpha N
  SO :: SAlpha O
  SP :: SAlpha P
  SQ :: SAlpha Q
  SR :: SAlpha R
  SS :: SAlpha S
  ST :: SAlpha T
  SU :: SAlpha U
  SV :: SAlpha V
  SW :: SAlpha W
  SX :: SAlpha X
  SY :: SAlpha Y
  SZ :: SAlpha Z

class IsAlpha (a :: Alpha) where
  promote :: SAlpha a
  demote :: SAlpha a -> Alpha

instance IsAlpha A where promote = SA; demote SA = A
instance IsAlpha B where promote = SB; demote SB = B
instance IsAlpha C where promote = SC; demote SC = C
instance IsAlpha D where promote = SD; demote SD = D
instance IsAlpha E where promote = SE; demote SE = E
instance IsAlpha F where promote = SF; demote SF = F
instance IsAlpha G where promote = SG; demote SG = G
instance IsAlpha H where promote = SH; demote SH = H
instance IsAlpha I where promote = SI; demote SI = I
instance IsAlpha J where promote = SJ; demote SJ = J
instance IsAlpha K where promote = SK; demote SK = K
instance IsAlpha L where promote = SL; demote SL = L
instance IsAlpha M where promote = SM; demote SM = M
instance IsAlpha N where promote = SN; demote SN = N
instance IsAlpha O where promote = SO; demote SO = O
instance IsAlpha P where promote = SP; demote SP = P
instance IsAlpha Q where promote = SQ; demote SQ = Q
instance IsAlpha R where promote = SR; demote SR = R
instance IsAlpha S where promote = SS; demote SS = S
instance IsAlpha T where promote = ST; demote ST = T
instance IsAlpha U where promote = SU; demote SU = U
instance IsAlpha V where promote = SV; demote SV = V
instance IsAlpha W where promote = SW; demote SW = W
instance IsAlpha X where promote = SX; demote SX = X
instance IsAlpha Y where promote = SY; demote SY = Y
instance IsAlpha Z where promote = SZ; demote SZ = Z
