-- Types.hs
-- Shared data types used across all modules.

module Types where

import Data.Time (Day)
import Data.Map (Map)

-- Tags that can be applied to any opportunity.
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

-- What kind of opportunity this is.
data OpportunityType
  = Internship
  | Job
  | ResearchPosition
  deriving (Show, Eq, Ord)

-- A single job/internship opportunity.
data Opportunity = Opportunity
  { oppId :: Int
  , oppTitle :: String
  , oppCompany :: String
  , oppDescription :: String
  , oppTags :: [Tag]
  , oppType :: OpportunityType
  , oppDeadline :: Maybe Day  -- Nothing = rolling deadline
  , oppURL :: String
  , oppSource :: String  -- e.g. "LinkedIn", "Handshake"
  } deriving (Show, Eq)

-- A user profile storing preferences for the recommendation algorithm.
data UserProfile = UserProfile
  { userName :: String
  , tagWeights :: Map Tag Double  -- higher = more interested in that tag
  , favorites :: [Int]  -- list of oppId's the user favorited
  } deriving (Show, Eq)

-- Search/filter options supplied by the user.
data SearchQuery = SearchQuery
  { queryKeyword  :: Maybe String -- free-text keyword
  , queryTags :: [Tag] -- must match all of these
  , queryType :: Maybe OpportunityType
  , queryRemote :: Maybe Bool -- Just True = remote only
  } deriving (Show, Eq)
