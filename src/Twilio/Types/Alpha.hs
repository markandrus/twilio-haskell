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
-- Copyright   :  (C) 2016- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module defines 'Alpha', a data type for the characters A through Z,
-- along with a singleton type.
-------------------------------------------------------------------------------
module Twilio.Types.Alpha
  ( Alpha(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import GHC.Generics (Generic)

import Twilio.Types.Sing (Sing, SingI(..), SingKind(..))

-- Alpha
-------------------------------------------------------------------------------

-- | Characters A through Z
data Alpha = A | B | C | D | E | F | G | H | I | J | K | L | M
           | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Binary, Bounded, Data, Enum, Eq, Generic, Ix, Ord, Read,
            Show, Typeable)

instance Hashable Alpha

instance NFData Alpha

data instance Sing (a :: Alpha) where
  SA :: Sing A
  SB :: Sing B
  SC :: Sing C
  SD :: Sing D
  SE :: Sing E
  SF :: Sing F
  SG :: Sing G
  SH :: Sing H
  SI :: Sing I
  SJ :: Sing J
  SK :: Sing K
  SL :: Sing L
  SM :: Sing M
  SN :: Sing N
  SO :: Sing O
  SP :: Sing P
  SQ :: Sing Q
  SR :: Sing R
  SS :: Sing S
  ST :: Sing T
  SU :: Sing U
  SV :: Sing V
  SW :: Sing W
  SX :: Sing X
  SY :: Sing Y
  SZ :: Sing Z

instance SingI A where sing = SA
instance SingI B where sing = SB
instance SingI C where sing = SC
instance SingI D where sing = SD
instance SingI E where sing = SE
instance SingI F where sing = SF
instance SingI G where sing = SG
instance SingI H where sing = SH
instance SingI I where sing = SI
instance SingI J where sing = SJ
instance SingI K where sing = SK
instance SingI L where sing = SL
instance SingI M where sing = SM
instance SingI N where sing = SN
instance SingI O where sing = SO
instance SingI P where sing = SP
instance SingI Q where sing = SQ
instance SingI R where sing = SR
instance SingI S where sing = SS
instance SingI T where sing = ST
instance SingI U where sing = SU
instance SingI V where sing = SV
instance SingI W where sing = SW
instance SingI X where sing = SX
instance SingI Y where sing = SY
instance SingI Z where sing = SZ

instance SingKind Alpha where
  type Demote Alpha = Alpha
  fromSing SA = A
  fromSing SB = B
  fromSing SC = C
  fromSing SD = D
  fromSing SE = E
  fromSing SF = F
  fromSing SG = G
  fromSing SH = H
  fromSing SI = I
  fromSing SJ = J
  fromSing SK = K
  fromSing SL = L
  fromSing SM = M
  fromSing SN = N
  fromSing SO = O
  fromSing SP = P
  fromSing SQ = Q
  fromSing SR = R
  fromSing SS = S
  fromSing ST = T
  fromSing SU = U
  fromSing SV = V
  fromSing SW = W
  fromSing SX = X
  fromSing SY = Y
  fromSing SZ = Z
