{-#LANGUAGE OverloadedStrings #-}

module Twilio.Internal.Parser where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import System.Locale

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

maybeReturn' :: Maybe (Maybe a) -> Parser (Maybe a)
maybeReturn' Nothing = return Nothing
maybeReturn' (Just Nothing) = mzero
maybeReturn' (Just ma) = return ma

maybeReturn'' :: Maybe (Maybe (Maybe a)) -> Parser (Maybe a)
maybeReturn'' Nothing = return Nothing
maybeReturn'' (Just Nothing) = return Nothing
maybeReturn'' (Just (Just Nothing)) = mzero
maybeReturn'' (Just (Just ma)) = return ma

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

maybeReturn :: (Monad m, MonadPlus m) => Maybe a -> m a
maybeReturn (Just a) = return a
maybeReturn Nothing  = mzero

newtype NonEmptyString = NonEmptyString { getNonEmptyString :: Maybe String }

instance FromJSON NonEmptyString where
  parseJSON (String "") = return $ NonEmptyString Nothing
  parseJSON (String v) = return . NonEmptyString . Just $ T.unpack v
  parseJSON _ = mzero
