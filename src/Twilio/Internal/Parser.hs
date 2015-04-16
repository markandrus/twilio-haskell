{-#LANGUAGE OverloadedStrings #-}

module Twilio.Internal.Parser where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format

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

filterEmpty :: Text -> Maybe Text
filterEmpty "" = Nothing
filterEmpty t = Just t

parseDate :: (Monad m, MonadPlus m) => Text -> m UTCTime
parseDate s =
  case parseTimeM True defaultTimeLocale "%F" (T.unpack s) of
    Just date -> return date
    Nothing   -> mzero

parseDateTime :: (Monad m, MonadPlus m) => Text -> m UTCTime
parseDateTime s =
  case parseTimeM True defaultTimeLocale "%a, %d %b %Y %T %z" (T.unpack s) of
    Just dateTime -> return dateTime
    Nothing       -> mzero

maybeReturn :: (Monad m, MonadPlus m) => Maybe a -> m a
maybeReturn (Just a) = return a
maybeReturn Nothing  = mzero

newtype NonEmptyText = NonEmptyText { getNonEmptyText :: Maybe Text }

instance FromJSON NonEmptyText where
  parseJSON (String "") = return $ NonEmptyText Nothing
  parseJSON (String v) = return . NonEmptyText $ Just v
  parseJSON _ = mzero

-- | Note that the parser only returns Nothing if the input
-- is Nothing. If the input is an incorrectly formatted
-- Text the parse will fail.
parseMaybeDateTime :: Maybe Text -> Parser (Maybe UTCTime)
parseMaybeDateTime (Just a) = Just <$> parseDateTime a
parseMaybeDateTime Nothing = return Nothing

valueToText :: Value -> Maybe Text
valueToText (String v) = Just v
valueToText _ = Nothing
