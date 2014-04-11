module Client
  ( Client(..)
  , client
  , AuthToken(getAuthToken)
  , parseAuthToken
  , baseURL
  , accountBaseURL
  , asClient
  ) where

import Account
import Types

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char (isLower, isNumber)
import Network.HTTP.Client

data Client = Client
  { accountSID :: !AccountSID
  , authToken  :: !AuthToken }
  deriving (Show, Eq)

client :: String -> String -> Maybe Client
client accountSID authToken = uncurry (liftM2 Client)
  (parseStringToSID accountSID :: Maybe AccountSID, parseAuthToken authToken)

newtype AuthToken = AuthToken { getAuthToken :: String }
  deriving (Show, Eq)

parseAuthToken :: String -> Maybe AuthToken
parseAuthToken token
  | length token == 32
  , all (\x -> isLower x || isNumber x) token
  = Just $ AuthToken token
  | otherwise
  = Nothing

baseURL :: String
baseURL = "https://api.twilio.com/2010-04-01"

accountBaseURL :: AccountSID -> String
accountBaseURL accountSID
  = baseURL ++ "/Accounts/" ++ getSID accountSID

asClient :: Client -> Request -> Request
asClient client =
  let user = C.pack . getSID       $ accountSID client
      pass = C.pack . getAuthToken $ authToken  client
  in  applyBasicAuth user pass
