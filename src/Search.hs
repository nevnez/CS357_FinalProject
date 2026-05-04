-- Backend/Search.hs
-- Search and keyword matching.
-- Assigned to: Nevaeh, Gael

module Search
  ( search
  , matchesQuery
  ) where

import Types
    ( Opportunity(oppTags, oppTitle, oppDescription, oppType),
      OpportunityType,
      SearchQuery(queryRemote, queryKeyword, queryTags, queryType),
      Tag(InPerson, Remote) )
import Data.Char (toLower)
import Data.List (isInfixOf)

-- Run a SearchQuery against a list of opportunities.
-- Returns only opportunities that satisfy ALL conditions in the query.
search :: SearchQuery -> [Opportunity] -> [Opportunity]
search query = filter (matchesQuery query)

-- Check whether a single opportunity satisfies a query.
-- Each condition is checked independently; all must pass.
matchesQuery :: SearchQuery -> Opportunity -> Bool
matchesQuery query opp =
  matchesKeyword (queryKeyword query) opp
  && matchesTags (queryTags   query) opp
  && matchesType (queryType   query) opp
  && matchesRemote (queryRemote query) opp

-- Keyword matches the title or description (case-insensitive).
matchesKeyword :: Maybe String -> Opportunity -> Bool
matchesKeyword Nothing _ = True   -- no keyword filter → always passes
matchesKeyword (Just kw) opp  =
  let needle = map toLower kw
      haystack = map toLower (oppTitle opp ++ " " ++ oppDescription opp)
  in needle `isInfixOf` haystack

-- All requested tags must be present on the opportunity.
matchesTags :: [Tag] -> Opportunity -> Bool
matchesTags required opp =
  all (`elem` oppTags opp) required

-- Opportunity type must match (if specified).
matchesType :: Maybe OpportunityType -> Opportunity -> Bool
matchesType Nothing _  = True
matchesType (Just want) opp = oppType opp == want

-- Remote/in-person filter.
matchesRemote :: Maybe Bool -> Opportunity -> Bool
matchesRemote Nothing _  = True
matchesRemote (Just True)  opp = Remote `elem` oppTags opp
matchesRemote (Just False) opp = InPerson `elem` oppTags opp
