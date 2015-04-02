module Twilio.Types.TwilioT where

import Twilio.Internal.Request

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
  | length token == 32
  , all (\x -> isLower x || isNumber x) token
  = return $ AuthToken token
  | otherwise
  = mzero

type Credentials = (AccountSID, AuthToken)

newtype TwilioT m a = TwilioT (Monad m => (Credentials, AccountSID) -> RequestT m a)
