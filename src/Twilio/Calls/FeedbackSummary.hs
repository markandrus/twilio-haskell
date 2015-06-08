{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Calls.FeedbackSummary
  ( -- * Resource
    FeedbackSummary(..)
  , FeedbackSummaryIssue(..)
    -- * Types
  , FeedbackSummarySID
  , Issue(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics

import Twilio.Internal.Parser
import Twilio.Types

data FeedbackSummary = FeedbackSummary
  { sid :: FeedbackSummarySID
  , startDate :: !Text
  , endDate   :: !Text
  , accountSID :: !AccountSID
  , includeSubaccounts            :: !Bool
  , status                        :: !FeedbackSummaryStatus
  , callCount                     :: !Int
  , callFeedbackCount             :: !Int
  , qualityScoreAverage           :: !Float
  , qualityScoreMedian            :: !Int
  , qualityScoreStandardDeviation :: !Int
  , issues                        :: ![FeedbackSummaryIssue]
  , dateCreated                   :: !UTCTime
  , dateUpdated                   :: !UTCTime
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON FeedbackSummary where
  parseJSON (Object v) = FeedbackSummary
    <$>  v .: "sid"
    <*>  v .: "start_date"
    <*>  v .: "end_date"
    <*>  v .: "account_sid"
    <*>  v .: "include_subaccounts"
    <*>  v .: "status"
    <*>  v .: "call_count"
    <*>  v .: "call_feedback_count"
    <*>  v .: "quality_score_average"
    <*>  v .: "quality_score_median"
    <*>  v .: "quality_score_standard_deviation"
    <*>  v .: "issues"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
  parseJSON _ = mzero

data FeedbackSummaryIssue = FeedbackSummaryIssue
  { description            :: !Issue
  , count                  :: !Int
  , percentageOfTotalCalls :: !Float
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON FeedbackSummaryIssue where
  parseJSON (Object v) = FeedbackSummaryIssue
    <$>  v .: "description"
    <*>  v .: "count"
    -- FIXME(mroberts): Need to parse this.
    <*>  v .: "percentage_of_total_calls"
  parseJSON _ = mzero

data FeedbackSummaryStatus
  = Queued
  | InProgress
  | Completed
  | Failed
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON FeedbackSummaryStatus where
  parseJSON (String "queued")      = return Queued
  parseJSON (String "in-progress") = return InProgress
  parseJSON (String "completed")   = return Completed
  parseJSON (String "failed")      = return Failed
  parseJSON _ = mzero
