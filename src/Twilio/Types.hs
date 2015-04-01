{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Twilio.Types
  ( APIVersion(..)
  , module Twilio.Types.AddressRequirement
  , module Twilio.Types.AuthToken
  , module Twilio.Types.Capability
  , module Twilio.Types.ISOCountryCode
  , module Twilio.Types.List
  , module Twilio.Types.PriceUnit
  , module Twilio.Types.SID
    -- * Misc
  , makeTwilioRequest
  , makeTwilioRequest'
  ) where

import Control.Monad
import Control.Monad.Reader.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Client

import Control.Monad.Twilio
import Twilio.Types.AddressRequirement
import Twilio.Types.AuthToken
import Twilio.Types.Capability
import Twilio.Types.ISOCountryCode
import Twilio.Types.List
import Twilio.Types.PriceUnit
import Twilio.Types.SID
import Twilio.Internal.Request

data APIVersion
  = API_2010_04_01
  | API_2008_08_01
  deriving Eq

instance Read APIVersion where
  readsPrec _ = \case
    "2010-04-01" -> return (API_2010_04_01, "")
    "2008-08-01" -> return (API_2008_08_01, "")
    _            -> mzero

instance Show APIVersion where
  show API_2010_04_01 = "2010-04-01"
  show API_2008_08_01 = "2008-08-01"

instance FromJSON APIVersion where
  parseJSON (String "2010-04-01") = return API_2010_04_01
  parseJSON (String "2008-08-01") = return API_2008_08_01
  parseJSON _ = mzero

{- List Resources -}

makeTwilioRequest' :: Monad m => String -> TwilioT m Request
makeTwilioRequest' suffix = do
  ((accountSID, authToken), _) <- ask
  let Just request = parseUrl (baseURL ++ suffix)
  return $ applyBasicAuth (C.pack $ getSID accountSID)
                          (C.pack $ getAuthToken authToken) request

makeTwilioRequest :: Monad m => String -> TwilioT m Request
makeTwilioRequest suffix = do
  ((_, _), accountSID) <- ask
  makeTwilioRequest' $ "/Accounts/" ++ getSID accountSID ++ suffix
