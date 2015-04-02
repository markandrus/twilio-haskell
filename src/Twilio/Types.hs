{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Twilio.Types
  ( APIVersion(..)
  , module X
    -- * Misc
  , makeTwilioRequest
  , makeTwilioRequest'
  , makeTwilioPOSTRequest
  , makeTwilioPOSTRequest'
  ) where

import Control.Monad
import Control.Monad.Reader.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Client

import Control.Monad.Twilio
import Twilio.Types.AddressRequirement as X
import Twilio.Types.AuthToken as X
import Twilio.Types.Capability as X
import Twilio.Types.ISOCountryCode as X
import Twilio.Types.List as X
import Twilio.Types.PriceUnit as X
import Twilio.Types.SID as X
import Twilio.Internal.Parser
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

makeTwilioPOSTRequest' :: Monad m
                       => String
                       -> [(C.ByteString, C.ByteString)]
                       -> TwilioT m Request
makeTwilioPOSTRequest' resourceURL params =
  makeTwilioRequest' resourceURL <&> urlEncodedBody params

makeTwilioPOSTRequest :: Monad m
                      => String
                      -> [(C.ByteString, C.ByteString)]
                      -> TwilioT m Request
makeTwilioPOSTRequest resourceURL params =
  makeTwilioRequest resourceURL <&> urlEncodedBody params
