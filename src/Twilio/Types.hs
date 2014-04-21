{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE PatternGuards #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Twilio.Types
  ( SID(..)
  , List(..)
  , PagingInformation(..)
  , AnsweredBy(..)
  , APIVersion(..)
  , Direction(..)
  , CallStatus(..)
  , PriceUnit(..)
  , Wrapper
  , wrap
  , (<&>)
  , filterEmpty
  , parseDateTime
  , safeRead
  , maybeReturn
  , maybeReturn'
  , AccountSID
  , AuthToken(getAuthToken)
  , parseAuthToken
  ) where

import Control.Monad (MonadPlus, mzero)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isLower, isNumber)
import Data.Text (pack, unpack)
import Data.Time.Format (parseTime)
import Data.Time.Clock (UTCTime)
import Debug.Trace (trace)
import Network.URI (URI, parseRelativeReference)
import System.Locale (defaultTimeLocale)

-- | 'SID's are 34 characters long and begin with two capital letters.
class SID a where

  -- | Get the 'wrap'ped constructor of the 'SID'.
  getSIDWrapper :: Wrapper (String -> a)

  -- | Get the two capital letters that prefix the 'SID'.
  getPrefix :: Const (Char, Char) a

  -- | Get the 'String' representation of the 'SID'.
  getSID :: a -> String

  -- | Parse a 'String' to an instance of the 'SID'.
  parseStringToSID :: String -> Maybe a
  parseStringToSID sid@(a:b:xs)
    | (a, b) == getConst (getPrefix :: Const (Char, Char) a)
    , length xs == 32
    , all (\x -> isLower x || isNumber x) xs
    = Just $ unwrap (getSIDWrapper :: Wrapper (String -> a)) sid
    | otherwise
    = Nothing
  parseStringToSID _ = Nothing

  -- | Parse a 'JSON' 'Value' to an instance of the 'SID'.
  parseJSONToSID :: Value -> Parser a
  parseJSONToSID (String v) = case parseStringToSID (unpack v) of
    Just sid -> return sid
    Nothing  -> mzero
  parseJSONToSID _ = mzero

class FromJSON b => List a b | a -> b where

  -- | Get the 'wrap'-ed constructor of the 'List'.
  getListWrapper :: Wrapper (Maybe PagingInformation -> [b] -> a)

  -- | The 'PagingInformation' for the 'List'.
  -- getPagingInformation :: a -> PagingInformation

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
  , firstPageURI :: !URI
    -- | The 'URI' for the next page of this list.
  , nextPageURI :: !(Maybe URI)
    -- | The 'URI' for the previous page of this list.
  , previousPageURI :: !(Maybe URI)
    -- | The 'URI' for the last page of this list.
  , lastPageURI :: !URI
  } deriving (Show, Eq)

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
   <*> (v .: "first_page_uri"    <&> parseRelativeReference
                                 >>= maybeReturn)
   <*> (v .: "next_page_uri"     <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "previous_page_uri" <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "last_page_uri"     <&> parseRelativeReference
                                 >>= maybeReturn)
  parseJSON _ = mzero

maybeReturn' :: Maybe (Maybe a) -> Parser (Maybe a)
maybeReturn' Nothing = return Nothing
maybeReturn' (Just Nothing) = mzero
maybeReturn' (Just ma) = return ma

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
  = API20100401
  | API20080801
  deriving Eq

instance Show APIVersion where
  show API20100401 = "2010-04-01"
  show API20080801 = "2008-08-01"

instance FromJSON APIVersion where
  parseJSON (String "2010-04-01") = return API20100401
  parseJSON (String "2008-08-01") = return API20080801
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

data Direction
  = Inbound
  | OutboundAPI
  | OutboundDial
  deriving Eq

instance Show Direction where
  show Inbound      = "inbound"
  show OutboundAPI  = "outbound-api"
  show OutboundDial = "outbound-dial"

instance FromJSON Direction where
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

-- | Account 'SID's are 34 characters long and begin with \"AC\".
newtype AccountSID = AccountSID { getAccountSID :: String }
  deriving (Show, Eq)

instance SID AccountSID where
  getSIDWrapper = wrap AccountSID
  getPrefix = Const ('A', 'C')
  getSID = getAccountSID

instance FromJSON AccountSID where
  parseJSON = parseJSONToSID

newtype AuthToken = AuthToken { getAuthToken :: String }
  deriving (Show, Eq)

parseAuthToken :: String -> Maybe AuthToken
parseAuthToken token
  | length token == 32
  , all (\x -> isLower x || isNumber x) token
  = Just $ AuthToken token
  | otherwise
  = Nothing

instance FromJSON AuthToken where
  parseJSON (String v) = maybeReturn . parseAuthToken $ unpack v
  parseJSON _ = mzero

