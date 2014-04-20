{-#LANGUAGE PatternGuards #-}

module Twilio.Client
  ( Client(..)
  , client
  , AuthToken(getAuthToken)
  , parseAuthToken
  , baseURL
  , accountBaseURL
  , asClient
  , runRequest
  ) where

import Twilio.Account
import Twilio.Types

import Control.Applicative ((<$>))
import Control.Monad
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isLower, isNumber)
import Data.Maybe (fromJust)
import Network.HTTP.Client (Request, applyBasicAuth, brConsume, newManager,
  parseUrl, responseBody, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)

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

runRequest :: FromJSON a => Client -> String -> IO a
runRequest client resourceURL = do
    manager <- newManager tlsManagerSettings
    withResponse request manager $ \response -> do
      bs <- LBS.fromChunks <$> brConsume (responseBody response)
      print bs
      return . fromJust $ decode bs
  where
    request = fromJust $ do
      req <- parseUrl $ accountBaseURL (accountSID client) ++ resourceURL
      return $ asClient client req
