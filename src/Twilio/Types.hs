{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE PatternGuards #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Twilio.Types
  ( -- * Twilio Monad
    Twilio
  , runTwilio
  , runTwilio'
    -- * Twilio Monad Transformer
  , TwilioT
  , runTwilioT
  , runTwilioT'
    -- * Making Requests
    -- $makingRequests
  , forAccount
  , request
  , requestForAccount
    -- * Twilio Exceptions
  , TwilioException(..)
    -- * Credentials
  , Credentials
  , parseCredentials
    -- ** Authentication Token
  , AuthToken
  , getAuthToken
  , parseAuthToken
    -- * System Identifiers (SIDs)
  , SID(getSID, parseSID)
    -- ** Resources
  , AccountSID
  , AddressSID
  , ApplicationSID
  , CallSID
  , ConnectAppSID
  , MessageSID
  , PhoneNumberSID
  , TranscriptionSID
  , RecordingSID
    -- * List Resources
  , List(..)
  , PagingInformation(..)
  , AnsweredBy(..)
  , APIVersion(..)
  , CallDirection(..)
  , CallStatus(..)
  , PriceUnit(..)
  , Wrapper
  , wrap
  , (<&>)
  , filterEmpty
  , parseDate
  , parseDateTime
  , safeRead
  , maybeReturn
  , maybeReturn'
  , maybeReturn''
    -- * Misc
  , ISOCountryCode(..)
  , AddressRequirement(..)
  , Capabilities
  , Capability(..)
  , parseCapabilitiesFromJSON
  , NonEmptyString(getNonEmptyString)
  ) where

import Control.Applicative
import Control.Exception (Exception, throw)
import Control.Monad (MonadPlus, liftM2, mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class (MonadReader(ask, local))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isLower, isNumber)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import Network.HTTP.Client (applyBasicAuth, brConsume, newManager, parseUrl,
  responseBody, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI (URI, parseRelativeReference)
import System.Locale (defaultTimeLocale)

import Twilio.Types.SID

-- | The set of 'Exception's that may be thrown when attempting to make
-- requests against Twilio's REST API.
data TwilioException
  = InvalidSID         !String
  | InvalidAuthToken   !String
  | InvalidCredentials
  deriving (Show, Eq, Typeable)

instance Exception TwilioException

{- Twilio Monad -}

-- | This monad allows you to make authenticated REST API requests to Twilio
-- using your 'AccountSID' and 'AuthToken'.
type Twilio = TwilioT IO

-- | Run zero or more REST API requests to Twilio.
runTwilio :: Credentials -> Twilio a -> IO a
runTwilio = runTwilioT

{- | Parse an account SID and authentication token before running zero or more
REST API requests to Twilio.

For example, you can fetch the 'Calls' resource in the 'IO' monad as follows:

>module Main where
>
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.Calls as Calls
>import Twilio.Types
>
>-- | Print calls.
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN")
>     $ Calls.get >>= liftIO . print
-}
runTwilio' :: IO String  -- ^ Account SID
           -> IO String  -- ^ Authentication Token
           -> Twilio a
           -> IO a
runTwilio' = runTwilioT'

{- Twilio Monad Transformer -}

-- | This monad transformer allows you to make authenticated REST API requests
-- to Twilio using your 'AccountSID' and 'AuthToken'.
newtype TwilioT m a = TwilioT {
    _runTwilioT :: (MonadThrow m, MonadIO m)
                => (Credentials, AccountSID)
                -> m a
  }

-- | Run zero or more REST API requests to Twilio, unwrapping the inner monad
-- @m@.
runTwilioT :: (MonadThrow m, MonadIO m) => Credentials -> TwilioT m a -> m a
runTwilioT credentials@(accountSID, _) = flip _runTwilioT (credentials, accountSID)

-- | Parse an account SID and authentication token before running zero or more
-- REST API requests to Twilio, unwrapping the inner monad @m@.
runTwilioT' :: (MonadThrow m, MonadIO m)
            => m String     -- ^ Account SID
            -> m String     -- ^ Authentication Token
            -> TwilioT m a
            -> m a
runTwilioT' getAccountSID getAuthToken twilio = do
  accountSID <- getAccountSID
  authToken  <- getAuthToken
  case parseCredentials accountSID authToken of
    Nothing -> throw InvalidCredentials
    Just credentials -> runTwilioT credentials twilio

instance Functor (TwilioT m) where
  fmap f ma = TwilioT $ \credentials -> do
    a <- _runTwilioT ma credentials
    return $ f a

liftTwilioT :: m a -> TwilioT m a
liftTwilioT m = TwilioT (const m)

instance Applicative m => Applicative (TwilioT m) where
  pure = liftTwilioT . pure
  f <*> v = TwilioT $ \r -> _runTwilioT f r <*> _runTwilioT v r

instance Alternative m => Alternative (TwilioT m) where
  empty = liftTwilioT empty
  m <|> n = TwilioT $ \r -> _runTwilioT m r <|> _runTwilioT n r

instance Monad m => Monad (TwilioT m) where
  return a = TwilioT (return . const a)
  m >>= k = TwilioT $ \client -> do
    a <- _runTwilioT m client
    _runTwilioT (k a) client

instance Monad m => MonadReader (Credentials, AccountSID) (TwilioT m) where
  ask = TwilioT return
  local f m = TwilioT $ _runTwilioT m . f

instance MonadTrans TwilioT where
  lift m = TwilioT $ const m

instance MonadIO m => MonadIO (TwilioT m) where
  liftIO = lift . liftIO

{- Making Requests -}

{- | Run zero or more Twilio REST API requests for an account that is a
sub-account of your own.

For example, you can fetch the 'Calls' resource for each of your account's
sub-accounts as follows:

>module Main where
>
>import Control.Monad (forM_)
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.Accounts as Accounts
>import Twilio.Calls as Calls
>import Twilio.Types
>
>-- | Print calls for each sub-account.
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN") $ do
>  subAccounts <- fmap getList Accounts.get
>  forM_ subAccounts $ \Account {Accounts.sid = subAccountSID} ->
>    forAccount subAccountSID Calls.get >>= liftIO . print
-}
forAccount :: (MonadThrow m, MonadIO m)
           => AccountSID   -- ^ Sub-Account SID
           -> TwilioT m a  -- ^ Zero or more Twilio REST API requests
           -> TwilioT m a
forAccount subAccountSID twilio = do
  (credentials, _) <- ask
  local (const (credentials, subAccountSID)) twilio

baseURL :: String
baseURL = "https://api.twilio.com/2010-04-01"

-- | Given a relative resource URL, make an authenticated GET request to
-- Twilio. /You should never need to use this function directly./
request :: (MonadIO m, FromJSON a) => String -> TwilioT m a
request resourceURL = do
  manager <- liftIO (newManager tlsManagerSettings)
  ((accountSID, authToken), _) <- ask
  let request = fromJust $ do
      req <- parseUrl (baseURL ++ resourceURL)
      return $ applyBasicAuth (C.pack $ getSID accountSID)
                              (C.pack $ getAuthToken authToken) req
  liftIO $ withResponse request manager $ \response -> do
    bs <- LBS.fromChunks <$> brConsume (responseBody response)
    putStrLn . C.unpack $ LBS.toStrict bs
    return . fromJust $ decode bs

-- | Given a relative resource URL, make an authenticated GET request to
-- Twilio for the specified account.
-- /You should never need to use this function directly./
requestForAccount :: (MonadIO m, FromJSON a) => String -> TwilioT m a
requestForAccount resourceURL = do
  (_, subAccountSID) <- ask
  request ("/Accounts/" ++ getSID subAccountSID ++ resourceURL)

{- Credentials -}

-- | Your 'AccountSID' and 'AuthToken' are used to make authenticated REST API
-- requests to Twilio.
type Credentials = (AccountSID, AuthToken)

-- | Parse an account SID and authentication token to 'Credential's.
parseCredentials
  :: String             -- ^ Account SID
  -> String             -- ^ Authentication Token
  -> Maybe Credentials
parseCredentials accountSID authToken = uncurry (liftM2 (,))
  ( parseSID accountSID :: Maybe AccountSID
  , parseAuthToken authToken )

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

maybeReturnE (Left _) = mzero
maybeReturnE (Right a) = return a

instance FromJSON AuthToken where
  parseJSON (String v) = parseAuthToken' $ unpack v
  parseJSON _ = mzero

{- List Resources -}

class FromJSON b => List a b | a -> b where

  -- | Get the 'wrap'-ed constructor of the 'List'.
  getListWrapper :: Wrapper (Maybe PagingInformation -> [b] -> a)

  {--- | The 'PagingInformation' for the 'List'.
  -- getPagingInformation :: a -> PagingInformation -}

  -- | The items in the 'List'.
  getList :: a -> [b]

  -- | The plural name for the items in the 'List'.
  getPlural :: Const String (a, b)

  -- | Parse a 'JSON' 'Value' to an instance of the 'List'.
  parseJSONToList :: Value -> Parser a
  parseJSONToList o@(Object v)
      =  unwrap (getListWrapper :: Wrapper (Maybe PagingInformation -> [b] -> a))
     <$> maybePagingInformation
     <*> (v .: pack (getConst (getPlural :: Const String (a, b))) :: Parser [b])
    where
      maybePagingInformation = case fromJSON o of
        Success pagingInformation -> return $ Just pagingInformation
        _ -> return Nothing
  parseJSONToList v = trace (show v) mzero

data PagingInformation = PagingInformation
  { -- | The current page number. Zero-indexed, so the first page is 0.
    pageNumber :: !Integer
    -- | The total number of pages.
  , numberOfPages :: !Integer
    -- | How many items are in each page.
  , pageSize :: !Integer
    -- | The total number of items in the list.
  , total :: !Integer
    -- | The position in the overall list of the first item in this page.
  , start :: !Integer
    -- | The position in the overall list of the last item in this page.
  , end :: !Integer
    -- | The 'URI' of the current page.
  , pageURI :: !URI
    -- | The 'URI' for the first page of this list.
  , firstPageURI :: !(Maybe URI)
    -- | The 'URI' for the next page of this list.
  , nextPageURI :: !(Maybe URI)
    -- | The 'URI' for the previous page of this list.
  , previousPageURI :: !(Maybe URI)
    -- | The 'URI' for the last page of this list.
  , lastPageURI :: !(Maybe URI)
  } deriving (Show, Eq, Ord)

instance FromJSON PagingInformation where
  parseJSON (Object v)
    =  PagingInformation
   <$>  v .: "page"
   <*>  v .: "num_pages"
   <*>  v .: "page_size"
   <*>  v .: "total"
   <*>  v .: "start"
   <*>  v .: "end"
   <*> (v .: "uri"               <&> parseRelativeReference
                                 >>= maybeReturn)
   <*> (v .: "first_page_uri"    <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "next_page_uri"     <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "previous_page_uri" <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "last_page_uri"     <&> fmap parseRelativeReference
                                 >>= maybeReturn')
  parseJSON _ = mzero

maybeReturn' :: Maybe (Maybe a) -> Parser (Maybe a)
maybeReturn' Nothing = return Nothing
maybeReturn' (Just Nothing) = mzero
maybeReturn' (Just ma) = return ma

maybeReturn'' :: Maybe (Maybe (Maybe a)) -> Parser (Maybe a)
maybeReturn'' Nothing = return Nothing
maybeReturn'' (Just Nothing) = return Nothing
maybeReturn'' (Just (Just Nothing)) = mzero
maybeReturn'' (Just (Just ma)) = return ma

-- NOTE(markandrus): Types used by Calls. If any of these end up being Calls-
-- specific, move them. APIVersion probably is not.

data PriceUnit
  = USD
  | EUR
  | JPY
  | OtherPriceUnit !String
  deriving Eq

-- NOTE(markandrus): This should be all ISO 4217 currencies. Maybe make a
-- separate library for these.
instance Show PriceUnit where
  show USD = "USD"
  show EUR = "EUR"
  show JPY = "JPY"
  show (OtherPriceUnit pu) = pu

instance FromJSON PriceUnit where
  parseJSON (String "USD") = return USD
  parseJSON (String "EUR") = return EUR
  parseJSON (String "JPY") = return JPY
  parseJSON (String t) = return . OtherPriceUnit $ unpack t
  parseJSON _ = mzero

data APIVersion
  = API_2010_04_01
  | API_2008_08_01
  deriving Eq

instance Read APIVersion where
  readsPrec _ = \s -> case s of
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

data CallStatus
  = Queued
  | Ringing
  | InProgress
  | Canceled
  | Completed
  | Failed
  | Busy
  | NoAnswer
  deriving Eq

instance Show CallStatus where
  show Queued     = "queued"
  show Ringing    = "ringing"
  show InProgress = "in-progress"
  show Canceled   = "canceled"
  show Completed  = "completed"
  show Failed     = "failed"
  show Busy       = "busy"
  show NoAnswer   = "no-answer"

instance FromJSON CallStatus where
  parseJSON (String "queued")      = return Queued
  parseJSON (String "ringing")     = return Ringing
  parseJSON (String "in-progress") = return InProgress
  parseJSON (String "canceled")    = return Canceled
  parseJSON (String "completed")   = return Completed
  parseJSON (String "failed")      = return Failed
  parseJSON (String "busy")        = return Busy
  parseJSON (String "no-answer")   = return NoAnswer
  parseJSON _ = mzero

data CallDirection
  = Inbound
  | OutboundAPI
  | OutboundDial
  deriving Eq

instance Show CallDirection where
  show Inbound      = "inbound"
  show OutboundAPI  = "outbound-api"
  show OutboundDial = "outbound-dial"

instance FromJSON CallDirection where
  parseJSON (String "inbound")       = return Inbound
  parseJSON (String "outbound-api")  = return OutboundAPI
  parseJSON (String "outbound-dial") = return OutboundDial
  parseJSON _ = mzero

data AnsweredBy
  = Human
  | Machine
  deriving Eq

instance Show AnsweredBy where
  show Human   = "human"
  show Machine = "machine"

instance FromJSON AnsweredBy where
  parseJSON (String "human")   = return Human
  parseJSON (String "machine") = return Machine
  parseJSON _ = mzero

newtype Wrapper a = Wrapper { unwrap :: a }

-- | 'wrap's a value so as not to break encapsulation.
wrap :: a -> Wrapper a
wrap = Wrapper

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

filterEmpty :: String -> Maybe String
filterEmpty "" = Nothing
filterEmpty s = Just s

parseDate :: (Monad m, MonadPlus m) => String -> m UTCTime
parseDate s =
  case parseTime defaultTimeLocale "%F" s of
    Just date -> return date
    Nothing   -> mzero

parseDateTime :: (Monad m, MonadPlus m) => String -> m UTCTime
parseDateTime s =
  case parseTime defaultTimeLocale "%a, %d %b %Y %T %z" s of
    Just dateTime -> return dateTime
    Nothing       -> mzero

safeRead :: (Monad m, MonadPlus m, Read a) => String -> m a
safeRead s = case reads s of
  [(a, "")] -> return a
  _         -> mzero

maybeReturn :: (Monad m, MonadPlus m) => Maybe a -> m a
maybeReturn (Just a) = return a
maybeReturn Nothing  = mzero

-- | Country codes in ISO 3166-1 alpha-2 format supported by Twilio.
data ISOCountryCode
  = AU  -- ^ Australia
  | AT  -- ^ Austria
  | BH  -- ^ Bahrain
  | BE  -- ^ Belgium
  | BR  -- ^ Brazil
  | BG  -- ^ Bulgaria
  | CA  -- ^ Canada
  | CY  -- ^ Cyprus
  | CZ  -- ^ Czech Republic
  | DK  -- ^ Denmark
  | DO  -- ^ Dominican Republic
  | SV  -- ^ El Salvador
  | EE  -- ^ Estonia
  | FI  -- ^ Finland
  | FR  -- ^ France
  | GR  -- ^ Greece
  | HK  -- ^ Hong Kong
  | IE  -- ^ Ireland
  | IL  -- ^ Israel
  | IT  -- ^ Italy
  | JP  -- ^ Japan
  | LV  -- ^ Latvia
  | LT  -- ^ Lithuania
  | LU  -- ^ Luxembourg
  | MT  -- ^ Malta
  | MX  -- ^ Mexico
  | NL  -- ^ The Netherlands
  | NO  -- ^ Norway
  | NZ  -- ^ New Zealand
  | PE  -- ^ Peru
  | PL  -- ^ Poland
  | PT  -- ^ Portugal
  | PR  -- ^ Puerto Rico
  | RO  -- ^ Romania
  | SK  -- ^ Slovakia
  | ZA  -- ^ South Africa
  | ES  -- ^ Spain
  | SE  -- ^ Sweden
  | CH  -- ^ Switzerland
  | GB  -- ^ United Kingdom
  | US  -- ^ United States
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON ISOCountryCode where
  parseJSON (String v) = safeRead $ unpack v
  parseJSON _ = mzero

data AddressRequirement
  = None
  | Any
  | Local
  | Foreign
  deriving (Bounded, Enum, Eq, Ord)

instance Read AddressRequirement where
  readsPrec _ = \s -> case s of
    "none"    -> return (None, "none")
    "any"     -> return (None, "any")
    "local"   -> return (None, "local")
    "foreign" -> return (None, "foregin")
    _         -> mzero

instance Show AddressRequirement where
  show None    = "none"
  show Any     = "any"
  show Local   = "local"
  show Foreign = "foreign"

instance FromJSON AddressRequirement where
  parseJSON (String "none")    = return None
  parseJSON (String "any")     = return Any
  parseJSON (String "local")   = return Local
  parseJSON (String "foreign") = return Foreign
  parseJSON _ = mzero

data Capability
  = Voice
  | SMS
  | MMS
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

parseCapabilitiesFromJSON :: Value -> Capabilities
parseCapabilitiesFromJSON (Object map)
  = let map' = fmap (\value -> case value of
                      Bool bool     -> bool
                      _             -> False) map
    in  foldr (\capability set ->
          if HashMap.lookupDefault False (pack $ show capability) map'
            then Set.insert capability set
            else set
        ) Set.empty [Voice, SMS, MMS]
parseCapabilitiesFromJSON _ = Set.empty

type Capabilities = Set Capability

newtype NonEmptyString = NonEmptyString { getNonEmptyString :: Maybe String }

instance FromJSON NonEmptyString where
  parseJSON (String "") = return $ NonEmptyString Nothing
  parseJSON (String v) = return . NonEmptyString . Just $ unpack v
  parseJSON _ = mzero
