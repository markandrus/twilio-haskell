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
import Data.Text (Text)
import qualified Data.Text as T

-- | Your authentication token is used to make authenticated REST API requests
-- to your Twilio account.
newtype AuthToken = AuthToken { getAuthToken' :: Text }
  deriving (Show, Eq, Ord)

-- | Get the 'Text' representation of an 'AuthToken'.
getAuthToken :: AuthToken -> Text
getAuthToken = getAuthToken'

-- | Parse a 'Text' to an 'AuthToken'.
parseAuthToken :: Text -> Maybe AuthToken
parseAuthToken = parseAuthToken'

parseAuthToken' :: MonadPlus m => Text -> m AuthToken
parseAuthToken' token
  | T.length token == 32
  , T.all (\x -> isLower x || isNumber x) token
  = return $ AuthToken token
  | otherwise
  = mzero

instance FromJSON AuthToken where
  parseJSON (String v) = parseAuthToken' v
  parseJSON _ = mzero
