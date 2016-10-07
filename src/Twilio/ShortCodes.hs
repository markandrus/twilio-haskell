{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.ShortCodes
  ( -- * Resource
    ShortCodes(..)
  , Twilio.ShortCodes.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson

import Control.Monad.Twilio
import Twilio.ShortCode
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data ShortCodes = ShortCodes
  { shortCodeList :: [ShortCode]
  } deriving (Show, Eq)

instance List ShortCodes ShortCode where
  getListWrapper = wrap (const ShortCodes)
  getList = shortCodeList
  getPlural = Const "short_codes"

instance FromJSON ShortCodes where
  parseJSON = parseJSONToList

instance Get0 ShortCodes where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/SMS/ShortCodes.json"

-- | Get 'ShortCodes'.
get :: MonadThrow m => TwilioT m ShortCodes
get = Resource.get
