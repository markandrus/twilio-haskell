{-#LANGUAGE PatternGuards #-}

module Twilio.Types.AuthToken
  ( -- * Authentication Token
    AuthToken
  , getAuthToken
  , parseAuthToken
  ) where

import Control.Monad
import Data.Aeson
import Data.Char
import qualified Data.Text as T

-- | Your authentication token is used to make authenticated REST API requests
-- to your Twilio account.
newtype AuthToken = AuthToken { getAuthToken' :: String }
  deriving (Show, Eq, Ord)

-- | Get the 'String' representation of an 'AuthToken'.
getAuthToken :: AuthToken -> String
getAuthToken = getAuthToken'

-- | Parse a 'String' to an 'AuthToken'.
parseAuthToken :: String -> Maybe AuthToken
parseAuthToken = parseAuthToken'

parseAuthToken' :: MonadPlus m => String -> m AuthToken
parseAuthToken' token
  | length token == 32
  , all (\x -> isLower x || isNumber x) token
  = return $ AuthToken token
  | otherwise
  = mzero

instance FromJSON AuthToken where
  parseJSON (String v) = parseAuthToken' $ T.unpack v
  parseJSON _ = mzero
