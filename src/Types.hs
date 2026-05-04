{-# LANGUAGE OverloadedStrings #-}
-- Types.hs
-- Shared data types used across all modules.

module Types where

import Data.Time (Day)
import Data.Map (Map)
import Data.Aeson
  ( FromJSON(..), ToJSON(..)
  , withText, withObject, (.:), (.:?)
  , object, (.=)
  )

-- Core types

data Tag
  = Software
  | Research
  | Remote
  | InPerson
  | Paid
  | Unpaid
  | PartTime
  | FullTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data OpportunityType
  = Internship
  | Job
  | ResearchPosition
  deriving (Show, Eq, Ord)

data Opportunity = Opportunity
  { oppId :: Int
  , oppTitle :: String
  , oppCompany :: String
  , oppDescription :: String
  , oppTags :: [Tag]
  , oppType :: OpportunityType
  , oppDeadline :: Maybe Day
  , oppURL :: String
  , oppSource :: String
  , oppIsWicPick :: Bool        -- True = hand-curated by WiC admin ⭐ (added feature)
  } deriving (Show, Eq)

data UserProfile = UserProfile
  { userName :: String
  , tagWeights :: Map Tag Double
  , favorites :: [Int]
  } deriving (Show, Eq)

data SearchQuery = SearchQuery
  { queryKeyword :: Maybe String
  , queryTags :: [Tag]
  , queryType :: Maybe OpportunityType
  , queryRemote :: Maybe Bool
  } deriving (Show, Eq)

-- JSON instances 

instance FromJSON Tag where
  parseJSON = withText "Tag" $ \t -> case t of
    "Software" -> pure Software
    "Research" -> pure Research
    "Remote" -> pure Remote
    "InPerson" -> pure InPerson
    "Paid" -> pure Paid
    "Unpaid" -> pure Unpaid
    "PartTime" -> pure PartTime
    "FullTime" -> pure FullTime
    other -> fail ("Unknown tag: " ++ show other)

instance ToJSON Tag where
  toJSON Software = "Software"
  toJSON Research = "Research"
  toJSON Remote = "Remote"
  toJSON InPerson = "InPerson"
  toJSON Paid = "Paid"
  toJSON Unpaid = "Unpaid"
  toJSON PartTime = "PartTime"
  toJSON FullTime = "FullTime"

instance FromJSON OpportunityType where
  parseJSON = withText "OpportunityType" $ \t -> case t of
    "Internship" -> pure Internship
    "Job" -> pure Job
    "ResearchPosition" -> pure ResearchPosition
    other -> fail ("Unknown type: " ++ show other)

instance ToJSON OpportunityType where
  toJSON Internship = "Internship"
  toJSON Job = "Job"
  toJSON ResearchPosition = "ResearchPosition"

instance FromJSON Opportunity where
  parseJSON = withObject "Opportunity" $ \v ->
    Opportunity
      <$> v .: "id"
      <*> v .: "title"
      <*> v .: "company"
      <*> v .: "description"
      <*> v .: "tags"
      <*> v .: "type"
      <*> v .:? "deadline"
      <*> v .: "url"
      <*> v .: "source"
      <*> v .: "isWicPick"

instance ToJSON Opportunity where
  toJSON opp = object
    [ "id" .= oppId opp
    , "title" .= oppTitle opp
    , "company" .= oppCompany opp
    , "description" .= oppDescription opp
    , "tags" .= oppTags opp
    , "type" .= oppType opp
    , "deadline" .= oppDeadline opp
    , "url" .= oppURL opp
    , "source" .= oppSource opp
    , "isWicPick" .= oppIsWicPick opp
    ]