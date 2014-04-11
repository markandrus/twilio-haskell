{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Twilio.Types
  ( SID(..)
  , List(..)
  , PagingInformation(..)
  , Wrapper
  , wrap
  ) where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isLower, isNumber)
import Data.Text (pack, unpack)
import Network.URI (URI, parseURI)

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

class FromJSON b => List a b where

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
  parseJSONToList _ = mzero

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
  , nextPageURI :: !URI
    -- | The 'URI' for the previous page of this list.
  , previousPageURI :: !URI
    -- | The 'URI' for the last page of this list.
  , lastPageURI :: !URI
  } deriving (Show, Eq)

instance FromJSON PagingInformation where
  parseJSON (Object v)
    =  PagingInformation
   <$> v .: "page"
   <*> v .: "numpages"
   <*> v .: "pagesize"
   <*> v .: "total"
   <*> v .: "start"
   <*> v .: "end"
   <*> v .: "Uri"
   <*> v .: "Firstpageuri"
   <*> v .: "Nextpageuri"
   <*> v .: "Previouspageuri"
   <*> v .: "Lastpageuri"
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

