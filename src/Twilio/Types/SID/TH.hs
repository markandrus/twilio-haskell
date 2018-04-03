{-#LANGUAGE CPP #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Types.SID.TH
-- Copyright   :  (C) 2017- Mark Andrus Roberts
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
import Language.Haskell.TH
#if MIN_VERSION_template_haskell(2,11,0)
  ( Bang(..)
  , SourceStrictness(..)
  , SourceUnpackedness(..)
#else
  ( Strict(..)
#endif
#if MIN_VERSION_template_haskell(2,12,0)
  , DerivClause(..)
#endif
  , Body(..)
  , Clause(..)
  , Con(..)
  , Dec(..)
  , DecsQ
  , Exp(..)
  , Pat(..)
  , Type(..)
  , mkName
  )

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
#if MIN_VERSION_template_haskell(2,11,0)
    derivedClasses = map (ConT . mkName)
#else
    derivedClasses = map mkName
#endif
      [ "Bounded"
      , "Data"
      , "Eq"
      , "FromJSON"
      , "Generic"
      , "Hashable"
      , "IsSID"
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

    newtypeDeclaration = NewtypeD [] makeSID []
#if MIN_VERSION_template_haskell(2,11,0)
      Nothing
#endif
      (RecC makeSID
        [ ( getSID
#if MIN_VERSION_template_haskell(2,11,0)
          , Bang NoSourceUnpackedness NoSourceStrictness
#else
          , NotStrict
#endif
          , sidType
          )
        ]
      )
#if MIN_VERSION_template_haskell(2,12,0)
      [DerivClause Nothing derivedClasses]
#else
      derivedClasses
#endif

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
