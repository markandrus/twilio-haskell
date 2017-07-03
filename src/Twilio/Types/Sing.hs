{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeInType #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.Sing
-- Copyright   :  (C) 2016- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module borrows the definitions for 'Sing', 'SingI', and 'SingKind' from
-- the singletons package.
-------------------------------------------------------------------------------
module Twilio.Types.Sing
  ( Sing
  , SingI(..)
  , SingKind(..)
  ) where

-- Sing
-------------------------------------------------------------------------------

data family Sing (a :: k)

-- SingI
-------------------------------------------------------------------------------

class SingI (a :: k) where
  sing :: Sing a

-- SingKind
-------------------------------------------------------------------------------

class SingKind k where
  type Demote k
  fromSing :: Sing (a :: k) -> Demote k
