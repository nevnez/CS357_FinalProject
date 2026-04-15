-- Backend/Filter.hs
-- Tag-based filtering helpers.
-- Assigned to: Nevaeh, Gael

module Filter
  ( filterByTags
  , filterByType
  , filterBySource
  , filterPaid
  , filterRemote
  ) where

import Types

-- | Keep only opportunities that have ALL of the given tags.
filterByTags :: [Tag] -> [Opportunity] -> [Opportunity]
filterByTags tags opps =
  filter (\opp -> all (`elem` oppTags opp) tags) opps

-- | Keep only opportunities of a given type.
filterByType :: OpportunityType -> [Opportunity] -> [Opportunity]
filterByType t = filter (\opp -> oppType opp == t)

-- | Keep only opportunities from a specific source website.
filterBySource :: String -> [Opportunity] -> [Opportunity]
filterBySource src = filter (\opp -> oppSource opp == src)

-- | Shortcut: paid opportunities only.
filterPaid :: [Opportunity] -> [Opportunity]
filterPaid = filterByTags [Paid]

-- | Shortcut: remote opportunities only.
filterRemote :: [Opportunity] -> [Opportunity]
filterRemote = filterByTags [Remote]
