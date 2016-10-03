{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Twilio.Types.List
  ( List(..)
  , PagingInformation(..)
  , Wrapper
  , wrap
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Data
import Data.Text (Text)
import Debug.Trace (trace)
import GHC.Generics
import Network.URI

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

class FromJSON b => List a b | a -> b where

  -- | Get the 'wrap'-ed constructor of the 'List'.
  getListWrapper :: Wrapper (Maybe PagingInformation -> [b] -> a)

  {--- | The 'PagingInformation' for the 'List'.
  -- getPagingInformation :: a -> PagingInformation -}

  -- | The items in the 'List'.
  getList :: a -> [b]

  -- | The plural name for the items in the 'List'.
  getPlural :: Const Text (a, b)

  -- | Parse a 'JSON' 'Value' to an instance of the 'List'.
  parseJSONToList :: Value -> Parser a
  parseJSONToList o@(Object v)
      =  unwrap (getListWrapper :: Wrapper (Maybe PagingInformation -> [b] -> a))
     <$> maybePagingInformation
     <*> (v .: getConst (getPlural :: Const Text (a, b)) :: Parser [b])
    where
      maybePagingInformation = case fromJSON o of
        Success pagingInformation -> return $ Just pagingInformation
        _ -> return Nothing
  parseJSONToList v = trace (show v) mzero

data PagingInformation = PagingInformation
  { -- | The current page number. Zero-indexed, so the first page is 0.
    pageNumber :: !Integer
    -- | The total number of pages.
  , numberOfPages :: !(Maybe Integer)
    -- | How many items are in each page.
  , pageSize :: !Integer
    -- | The total number of items in the list.
  , total :: !(Maybe Integer)
    -- | The position in the overall list of the first item in this page.
  , start :: !Integer
    -- | The position in the overall list of the last item in this page.
  , end :: !Integer
    -- | The 'URI' of the current page.
  , pageURI :: !(Maybe URI)
    -- | The 'URI' for the first page of this list.
  , firstPageURI :: !(Maybe URI)
    -- | The 'URI' for the next page of this list.
  , nextPageURI :: !(Maybe URI)
    -- | The 'URI' for the previous page of this list.
  , previousPageURI :: !(Maybe URI)
    -- | The 'URI' for the last page of this list.
  , lastPageURI :: !(Maybe URI)
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance FromJSON PagingInformation where
  parseJSON (Object v)
    =  PagingInformation
   <$>  v .: "page"
   <*>  v .:? "num_pages"
   <*>  v .: "page_size"
   <*>  v .:? "total"
   <*>  v .: "start"
   <*>  v .: "end"
   <*> (v .: "uri"               <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "first_page_uri"    <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "next_page_uri"     <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "previous_page_uri" <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .:? "last_page_uri"    <&> fmap parseRelativeReference
                                 >>= maybeReturn')
  parseJSON _ = mzero

maybeReturn' :: Maybe (Maybe a) -> Parser (Maybe a)
maybeReturn' Nothing = return Nothing
maybeReturn' (Just Nothing) = mzero
maybeReturn' (Just ma) = return ma

newtype Wrapper a = Wrapper { unwrap :: a }

-- | 'wrap's a value so as not to break encapsulation.
wrap :: a -> Wrapper a
wrap = Wrapper
