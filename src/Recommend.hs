-- Backend/Recommend.hs
-- Weighted tag recommendation algorithm.
-- Assigned to: Gael, Nevaeh
--
-- Algorithm:
--   Each opportunity gets a score = sum of weights for its tags.
--   Opportunities with no matching tags score 0 but are still shown
--   at the bottom (so users can still discover new things).

module Recommend
  ( scoreOpportunity
  , recommend
  , defaultProfile
  ) where

import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- | Compute a relevance score for one opportunity given a user profile.
-- Score = sum of tag weights for every tag the opportunity has.
scoreOpportunity :: UserProfile -> Opportunity -> Double
scoreOpportunity profile opp =
  sum [ Map.findWithDefault 0.0 tag (tagWeights profile)
      | tag <- oppTags opp
      ]

-- | Return opportunities sorted by relevance score, highest first.
recommend :: UserProfile -> [Opportunity] -> [Opportunity]
recommend profile =
  sortBy (comparing (Down . scoreOpportunity profile))

-- | A blank user profile with equal weight (1.0) for every tag.
-- Use this when no saved profile exists yet.
defaultProfile :: String -> UserProfile
defaultProfile name = UserProfile
  { userName   = name
  , tagWeights = Map.fromList [(tag, 1.0) | tag <- [minBound..maxBound]]
  , favorites  = []
  }

-- | Update a profile: increase weight for a given tag (e.g. after a favorite).
boostTag :: Tag -> Double -> UserProfile -> UserProfile
boostTag tag amount profile =
  profile { tagWeights = Map.insertWith (+) tag amount (tagWeights profile) }

-- | Add an opportunity to the user's favorites and boost its tags.
addFavorite :: Opportunity -> UserProfile -> UserProfile
addFavorite opp profile =
  let boosted = foldr (\tag p -> boostTag tag 0.5 p) profile (oppTags opp)
  in boosted { favorites = oppId opp : favorites boosted }
