-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.SID.TH
-- Copyright   :  (C) 2016- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- The code for defining SIDs is highly-repetitive and follows a pattern. This
-- module codifies that pattern.
-------------------------------------------------------------------------------
module Twilio.Types.SID.TH
  ( createSID
  ) where

import Data.Monoid ((<>))
import Language.Haskell.TH (Bang(..), Body(..), Clause(..), Con(..), Dec(..),
                            DecsQ, Exp(..), Pat(..), SourceStrictness(..),
                            SourceUnpackedness(..), Type(..), mkName)

import Twilio.Types.Alpha (Alpha)

-- createSID
-------------------------------------------------------------------------------

-- | Create a 'SID' newtype and declare all the useful instances we expect of a
-- 'SID'. For example,
--
-- @@@
-- createSID A C "Account"
-- @@@
createSID
  :: Alpha   -- ^ The first letter prefixing the 'SID'
  -> Alpha   -- ^ The second letter prefixing the 'SID'
  -> String  -- ^ The name of the resource this 'SID' will identify
  -> DecsQ
createSID a b resource = pure
    (
      [ newtypeDeclaration
      ]
     <> smartConstructor
    )
  where
    derivedClasses = map (ConT . mkName)
      [ "Bounded"
      , "Data"
      , "Eq"
      , "FromJSON"
      , "Generic"
      , "Hashable"
      , "IsString"
      , "Ix"
      , "NFData"
      , "Ord"
      , "Read"
      , "Show"
      , "ToJSON"
      , "Typeable"
      ]

    getSID = mkName $ "get" <> resource <> "SID"

    makeSID = mkName $ resource <> "SID"

    newtypeDeclaration = NewtypeD [] makeSID [] Nothing
      (RecC makeSID
        [ ( getSID
          , Bang NoSourceUnpackedness NoSourceStrictness
          , sidType
          )
        ]
      )
      derivedClasses

    sidType = AppT
      (AppT (ConT $ mkName "SID")
        (PromotedT . mkName $ show a))
        (PromotedT . mkName $ show b)

    smartConstructor =
      [ SigD (mkName $ "mk" <> resource <> "SID")
          (AppT
            (AppT
              ArrowT
              (ConT $ mkName "Word64"))
            (AppT
              (AppT
                ArrowT
                (ConT $ mkName "Word64"))
              (ConT makeSID)))
      , FunD (mkName $ "mk" <> resource <> "SID")
          [ Clause [VarP $ mkName "word1", VarP $ mkName "word2"]
              (NormalB (AppE (ConE makeSID)
                (AppE (AppE (ConE $ mkName "SID")
                  (VarE $ mkName "word1"))
                  (VarE $ mkName "word2"))))
              []
          ]
      ]
