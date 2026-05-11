{-# LANGUAGE OverloadedStrings #-}
-- Admin.hs
-- Shared admin logic for both Main.hs and UI.hs

module Admin
  ( addWicJob
  , removeWicJob
  , loadWicJobs
  ) where

import Types
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory (doesFileExist)

wicJobsFile :: FilePath
wicJobsFile = "wic-jobs.json"

cacheFile :: FilePath
cacheFile = "opportunities.json"

loadWicJobs :: IO [Opportunity]
loadWicJobs = do
  exists <- doesFileExist wicJobsFile
  if not exists
    then return []
    else do
      result <- eitherDecodeFileStrict wicJobsFile
      case result of
        Left _ -> return []
        Right opps -> return opps

addWicJob :: [Opportunity] -> String -> String -> String -> String -> String -> [Tag] -> OpportunityType -> IO [Opportunity]
addWicJob opps title company desc url source tags oppType = do
  let newOpp = Opportunity
        { oppId = length opps + 1
        , oppTitle = title
        , oppCompany = company
        , oppDescription = desc
        , oppTags = tags
        , oppType = oppType
        , oppDeadline = Nothing
        , oppURL = url
        , oppSource = source
        , oppIsWicPick = True
        }
  existing <- loadWicJobs
  encodeFile wicJobsFile (existing ++ [newOpp])
  encodeFile cacheFile (opps ++ [newOpp])
  return (opps ++ [newOpp])

removeWicJob :: [Opportunity] -> Int -> IO [Opportunity]
removeWicJob opps targetId = do
  let updated = filter (\o -> not (oppIsWicPick o && oppId o == targetId)) opps
      wicOnly = filter oppIsWicPick updated
  encodeFile wicJobsFile wicOnly
  encodeFile cacheFile updated
  return updated