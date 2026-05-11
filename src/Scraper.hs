{-# LANGUAGE OverloadedStrings #-}
-- Scraper.hs
-- Fetches real job listings from RemoteOK (free, no key) and
-- USAJobs (federal jobs in NM, requires API key in config.json).
-- Also loads WiC admin-curated jobs from wic-jobs.json.
-- Can't get USAJobs working. Running into TLS issues with WSL's network stack.

module Scraper
  ( scrapeAll
  , refreshAll
  , loadFromCache
  ) where

import Types
import Data.Aeson( eitherDecodeFileStrict, encodeFile, eitherDecode, Value(..), (.:), (.:?) )
import Data.Aeson.Types (parseMaybe, Parser)
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody, setRequestHeader)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC
import System.Directory (doesFileExist)
import System.IO.Error (catchIOError)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (isInfixOf)
import Data.Char (toLower)

-- Config]
cacheFile :: FilePath
cacheFile = "opportunities.json"

wicJobsFile :: FilePath
wicJobsFile = "wic-jobs.json"

configFile :: FilePath
configFile = "config.json"

-- CS-related keywords for filtering RemoteOK results
-- FYI: All remote jobs so location doesnt matter
csKeywords :: [String]
csKeywords =
  [ "software", "engineer", "engineering", "developer", "dev"
  , "programming", "backend", "frontend", "fullstack", "full-stack"
  , "data", "python", "javascript", "haskell", "java"
  , "research", "intern", "junior", "computer", "api"
  , "devops", "cloud", "security", "cyber", "ai", "ml"
  ]

-- Config loading
data AppConfig = AppConfig
  { usaJobsApiKey :: String
  , usaJobsEmail  :: String
  } deriving (Show)

loadConfig :: IO (Maybe AppConfig)
loadConfig = do
  exists <- doesFileExist configFile
  if not exists
    then do
      putStrLn "[Config] No config.json found. USAJobs scraper will be skipped."
      putStrLn "[Config] Create config.json with: {\"usajobsApiKey\": \"...\", \"usajobsEmail\": \"...\"}"
      return Nothing
    else do
      result <- eitherDecodeFileStrict configFile
      case result of
        Left err -> do
          putStrLn ("[Config] Could not parse config.json: " ++ err)
          return Nothing
        Right val -> return (parseMaybe parseConfig val)
  where
    parseConfig (Object o) = AppConfig
      <$> o .: "usajobsApiKey"
      <*> o .: "usajobsEmail"
    parseConfig _ = fail "Expected object"

-- Public API 

-- Load from cache on startup 
loadFromCache :: IO [Opportunity]
loadFromCache = do
  exists <- doesFileExist cacheFile
  if exists
    then do
      result <- eitherDecodeFileStrict cacheFile
      case result of
        Left err  -> do
          putStrLn ("[Scraper] Cache error: " ++ err)
          return []
        Right opps -> do
          putStrLn ("[Scraper] Loaded " ++ show (length opps) ++ " cached opportunities.")
          return opps
    else do
      putStrLn "[Scraper] No cache yet. Hit Refresh (6) to fetch live data."
      return []

-- Scrape all sources, combine, save cache, return results.
scrapeAll :: IO [Opportunity]
scrapeAll = do
  -- Load config for USAJobs
  config <- loadConfig

  putStrLn "[Scraper] Fetching from RemoteOK..."
  remoteOpps <- scrapeRemoteOK

  let usaOpps = []

  putStrLn "[Scraper] Loading WiC curated jobs..."
  wicOpps <- loadWicJobs

  -- WiC picks first, then USAJobs (local/federal), then RemoteOK (remote)
  let allOpps = assignIds (wicOpps ++ usaOpps ++ remoteOpps)
  saveToCache allOpps
  putStrLn ("[Scraper] Done! " ++ show (length allOpps) ++ " total opportunities.")
  return allOpps

refreshAll :: IO [Opportunity]
refreshAll = scrapeAll

-- RemoteOK

scrapeRemoteOK :: IO [Opportunity]
scrapeRemoteOK = do
  result <- fetchRemoteOK
  case result of
    Left err -> do
      putStrLn ("[RemoteOK] Failed: " ++ err)
      return []
    Right body ->
      case eitherDecode body of
        Left err -> do
          putStrLn ("[RemoteOK] Parse error: " ++ err)
          return []
        Right values -> do
          let jobs = drop 1 values  -- first element is legal notice
              opps = mapMaybe parseRemoteOKJob jobs
              relevant = filter isCSRelevant opps
          putStrLn ("[RemoteOK] " ++ show (length relevant)
                    ++ " CS-relevant jobs from " ++ show (length opps) ++ " total.")
          return relevant

fetchRemoteOK :: IO (Either String LBS.ByteString)
fetchRemoteOK = catchIOError
  (do req  <- parseRequest "https://remoteok.com/api"
      let req' = setRequestHeader "User-Agent" ["WiC-CLI/1.0 UNM-WomenInComputing"] req
      resp <- httpLBS req'
      return (Right (getResponseBody resp)))
  (\e -> return (Left (show e)))

parseRemoteOKJob :: Value -> Maybe Opportunity
parseRemoteOKJob = parseMaybe go
  where
    go :: Value -> Parser Opportunity
    go (Object o) = do
      jobId <- o .: "id"
      title <- o .: "position"
      company <- o .: "company"
      desc <- o .: "description"
      rawTags <- o .: "tags"
      url <- o .: "url"
      loc <- o .:? "location"
      salMin <- o .:? "salary_min"
      salMax <- o .:? "salary_max"
      let location = fromMaybe "Remote" (loc :: Maybe String)
          isPaid = maybe False (> (0 :: Int)) salMin
                  || maybe False (> (0 :: Int)) salMax
          isRemote = null location
                  || map toLower location == "remote"
                  || "remote" `isInfixOf` map toLower location
          tags = buildTags (rawTags :: [String]) isPaid isRemote title desc
      return Opportunity
        { oppId = read jobId
        , oppTitle = title
        , oppCompany = company
        , oppDescription = stripHTML desc
        , oppTags = tags
        , oppType = inferType title desc
        , oppDeadline = Nothing
        , oppURL = url
        , oppSource = "RemoteOK"
        , oppIsWicPick = False
        }
    go _ = fail "Not an object"

isCSRelevant :: Opportunity -> Bool
isCSRelevant opp =
  let combined = map toLower (oppTitle opp ++ " " ++ oppDescription opp)
  in any (`isInfixOf` combined) csKeywords
   || Software `elem` oppTags opp
   || Research `elem` oppTags opp


-- WiC Curated Jobs

loadWicJobs :: IO [Opportunity]
loadWicJobs = do
  exists <- doesFileExist wicJobsFile
  if not exists
    then return []
    else do
      result <- eitherDecodeFileStrict wicJobsFile
      case result of
        Left  err  -> do
          putStrLn ("[WiC] Could not load wic-jobs.json: " ++ err)
          return []
        Right opps -> do
          putStrLn ("[WiC] Loaded " ++ show (length (opps :: [Opportunity])) ++ " curated jobs.")
          return opps

-- Helpers

assignIds :: [Opportunity] -> [Opportunity]
assignIds = zipWith (\i o -> o { oppId = i }) [1..]

saveToCache :: [Opportunity] -> IO ()
saveToCache opps = do
  encodeFile cacheFile opps
  putStrLn ("[Scraper] Saved " ++ show (length opps) ++ " opportunities to cache.")

buildTags :: [String] -> Bool -> Bool -> String -> String -> [Tag]
buildTags rawTags isPaid isRemote title desc =
  let t = map toLower (title ++ " " ++ desc ++ " " ++ unwords rawTags)
      remote = [Remote | isRemote]
      inperson = [InPerson | not isRemote]
      paid = [Paid | isPaid || not ("unpaid" `isInfixOf` t)]
      research = [Research | "research" `isInfixOf` t]
      software = [Software | "software"   `isInfixOf` t
                           || "engineer"  `isInfixOf` t
                           || "developer" `isInfixOf` t]
      parttime = [PartTime | "part-time" `isInfixOf` t || "part time" `isInfixOf` t]
      fulltime = [FullTime | "full-time" `isInfixOf` t || "full time" `isInfixOf` t]
  in  concat [remote, inperson, paid, research, software, parttime, fulltime]

inferType :: String -> String -> OpportunityType
inferType title desc =
  let t = map toLower (title ++ " " ++ desc)
  in if "intern" `isInfixOf` t then Internship
      else if "research" `isInfixOf` t then ResearchPosition
      else Job

stripHTML :: String -> String
stripHTML [] = []
stripHTML ('<':rest) = stripHTML (dropWhile (/= '>') rest)
stripHTML (c :rest) = c : stripHTML rest

urlEncode :: String -> String
urlEncode [] = []
urlEncode (' ':cs) = '+' : urlEncode cs
urlEncode (c  :cs)
  | c `elem` safe = c : urlEncode cs
  | otherwise = '%' : toHex c ++ urlEncode cs
  where safe = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~" :: String

toHex :: Char -> String
toHex c =
  let n = fromEnum c
      d = "0123456789ABCDEF"
  in  [d !! (n `div` 16), d !! (n `mod` 16)]