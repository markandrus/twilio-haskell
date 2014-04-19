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
  , Wrapper
  , wrap
  , (<&>)
  , filterEmpty
  , parseDateTime
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
import Network.URI (URI, parseURI)
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
  getListWrapper :: Wrapper (PagingInformation -> [b] -> a)

  -- | The 'PagingInformation' for the 'List'.
  getPagingInformation :: a -> PagingInformation

  -- | The items in the 'List'.
  getItems :: a -> [b]

  -- | The plural name for the items in the 'List'.
  getPlural :: Const String (a, b)

  -- | Parse a 'JSON' 'Value' to an instance of the 'List'.
  parseJSONToList :: Value -> Parser a
  parseJSONToList o@(Object v)
    =  unwrap (getListWrapper :: Wrapper (PagingInformation -> [b] -> a))
   <$> (parseJSON o :: Parser PagingInformation)
   <*> (v .: pack (getConst (getPlural :: Const String (a, b))) :: Parser [b])
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
  , pageURI :: !String
    -- | The 'URI' for the first page of this list.
  , firstPageURI :: !String
    -- | The 'URI' for the next page of this list.
  , nextPageURI :: !(Maybe String)
    -- | The 'URI' for the previous page of this list.
  , previousPageURI :: !(Maybe String)
    -- | The 'URI' for the last page of this list.
  , lastPageURI :: !String
  } deriving (Show, Eq)

instance FromJSON PagingInformation where
  parseJSON (Object v)
    =  PagingInformation
   <$> v .: "page"
   <*> v .: "num_pages"
   <*> v .: "page_size"
   <*> v .: "total"
   <*> v .: "start"
   <*> v .: "end"
   <*> v .: "uri"
   <*> v .: "first_page_uri"
   <*> v .: "next_page_uri"
   <*> v .: "previous_page_uri"
   <*> v .: "last_page_uri"
  parseJSON _ = mzero

instance FromJSON URI where
  parseJSON (String v) = case parseURI (unpack v) of
    Just sid -> return sid
    Nothing  -> mzero
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
