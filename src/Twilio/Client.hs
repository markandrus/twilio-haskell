module Twilio.Client
  ( Client(..)
  , client
  , baseURL
  , accountBaseURL
  , asClient
  , runRequest
  ) where

import Twilio.Types

import Control.Applicative ((<$>))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
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
      putStrLn . C.unpack $ LBS.toStrict bs
      return . fromJust $ decode bs
  where
    request = fromJust $ do
      req <- parseUrl resourceURL
      return $ asClient client req
