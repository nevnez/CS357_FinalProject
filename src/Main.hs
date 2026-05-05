-- Main.hs
-- Entry point: main loop wiring the scraper, backend, and UI together.
-- ====================================================================
  -- DO NOT PUSH .gitignore
-- ==================================================================== 
-- user/admin system. Admins are defined in the admins list below.
-- No password required 

module Main where

import Types
import Scraper (loadFromCache, refreshAll)
import Search (search)
import Filter (filterByTags, filterPaid, filterRemote)
import Recommend (recommend, defaultProfile)
import Deadline (sortByDeadline, upcomingDeadlines)
import Display
import Data.Time (getCurrentTime, utctDay, Day)

import Data.Aeson (eitherDecodeFileStrict, encodeFile, object, (.=), Value(..))
import Data.Aeson.Types (parseMaybe, (.:))
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering))
import System.IO (hFlush, stdout)

-- =========== Admin list ============
-- add username to grant admin access.
-- Case-insensitive

admins :: [String]
admins = ["WICAdmin14", "nevnez14", "kiana14", "gael14"]

isAdmin :: String -> Bool
isAdmin name = map toLower name `elem` map (map toLower) admins

-- WiC jobs file

wicJobsFile :: FilePath
wicJobsFile = "wic-jobs.json"


main :: IO ()
main = do
  putStrLn "╔══════════════════════════════════════╗"
  putStrLn "║   WiC Opportunities CLI              ║"
  putStrLn "║   UNM Women in Computing             ║"
  putStrLn "╚══════════════════════════════════════╝"
  putStrLn "  Enter your name: "
  putStr   "> "
  hFlush stdout
  name <- getLine
  opps <- loadFromCache
  today <- utctDay <$> getCurrentTime
  let profile = defaultProfile name
  if isAdmin name
    then do
      putStrLn ("  Welcome, " ++ name ++ "! [ADMIN]")
      adminLoop today opps profile
    else do
      putStrLn ("  Welcome, " ++ name ++ "!")
      userLoop today opps profile

-- Main REPL loop for USERS
userLoop :: Day -> [Opportunity] -> UserProfile -> IO ()
userLoop today opps profile = do
  displayMenu False
  choice <- getLine
  case choice of
    "1" -> do
      displayList today opps
      userLoop today opps profile

    "2" -> do
      kw <- promptUser "Enter keyword:"
      let results = search (emptyQuery { queryKeyword = Just kw }) opps
      displayList today results
      userLoop today opps profile

    "3" -> do
      putStrLn "Tags: Software, Research, Remote, InPerson, Paid, Unpaid, PartTime, FullTime"
      tagStr <- promptUser "Enter tag:"
      case parseTag tagStr of
        Nothing -> putStr ("Unknown tag: " ++ tagStr)
        Just tag -> displayList today (filterByTags [tag] opps)
      userLoop today opps profile  

    "4" -> do
      daysStr <- promptUser "Show deadlines within how many days? (default 30):"
      let n = if null daysStr then 30 else read daysStr
      let upcoming = upcomingDeadlines today n (sortByDeadline opps)
      displayList today upcoming
      userLoop today opps profile

    "5" -> do
      let ranked = recommend profile opps
      displayList today ranked
      userLoop today opps profile

    "6" -> do
      fresh <- refreshAll
      userLoop today fresh profile

    "0" -> putStrLn "Goodbye!"

    _ -> do
      putStrLn "Invalid option, try again."
      userLoop today opps profile

-- Main REPL loop for ADMIN

adminLoop :: Day -> [Opportunity] -> UserProfile -> IO ()
adminLoop today opps profile = do
  displayMenu True
  choice <- getLine
  case choice of
    "1" -> do
      displayList today opps
      adminLoop today opps profile
 
    "2" -> do
      kw <- promptUser "Enter keyword:"
      let results = search (emptyQuery { queryKeyword = Just kw }) opps
      displayList today results
      adminLoop today opps profile
 
    "3" -> do
      putStrLn "Tags: Software, Research, Remote, InPerson, Paid, Unpaid, PartTime, FullTime"
      tagStr <- promptUser "Enter tag:"
      case parseTag tagStr of
        Nothing  -> putStrLn ("Unknown tag: " ++ tagStr)
        Just tag -> displayList today (filterByTags [tag] opps)
      adminLoop today opps profile
 
    "4" -> do
      daysStr <- promptUser "Show deadlines within how many days? (default 30):"
      let n = if null daysStr then 30 else read daysStr
      displayList today (upcomingDeadlines today n (sortByDeadline opps))
      adminLoop today opps profile
 
    "5" -> do
      displayList today (recommend profile opps)
      adminLoop today opps profile
 
    "6" -> do
      fresh <- refreshAll
      adminLoop today fresh profile
 
    "7" -> do
      fresh <- addWicJob opps
      adminLoop today fresh profile
 
    "8" -> do
      fresh <- removeWicJob opps
      adminLoop today fresh profile
 
    "0" -> putStrLn "Goodbye!"
 
    _ -> do
      putStrLn "Invalid option."
      adminLoop today opps profile

-- Admin Actions

addWicJob :: [Opportunity] -> IO [Opportunity]
addWicJob opps = do
  putStrLn "\n── Add WiC Curated Job ──"
  title <- promptUser "Job title:"
  company <- promptUser "Company:"
  desc <- promptUser "Description:"
  url <- promptUser "URL:"
  source <- promptUser "Source (e.g. LinkedIn, Handshake):"
  putStrLn "Tags (comma separated): Software, Research, Remote, InPerson, Paid, Unpaid, PartTime, FullTime"
  tagStr <- promptUser "Tags:"
  putStrLn "Type: 1=Internship, 2=Job, 3=ResearchPosition"
  typeStr <- promptUser "Type:"
 
  let tags = parseTags tagStr
      oppType = case typeStr of
                  "1" -> Internship
                  "3" -> ResearchPosition
                  _   -> Job
      newOpp = Opportunity
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
 
  -- Load existing WiC jobs, append, save
  existing <- loadWicJobs
  let updated = existing ++ [newOpp]
  encodeFile wicJobsFile updated
  putStrLn ("[Admin] Added '" ++ title ++ "' as a WiC Pick!")
  return (opps ++ [newOpp])
 
removeWicJob :: [Opportunity] -> IO [Opportunity]
removeWicJob opps = do
  putStrLn "\n── Remove WiC Curated Job ──"
  let wicJobs = filter oppIsWicPick opps
  if null wicJobs
    then do
      putStrLn "No WiC curated jobs to remove."
      return opps
    else do
      putStrLn "Current WiC Picks:"
      mapM_ (\o -> putStrLn ("  " ++ show (oppId o) ++ ". " ++ oppTitle o ++ " @ " ++ oppCompany o)) wicJobs
      idStr <- promptUser "Enter ID to remove:"
      let targetId = read idStr :: Int
          updated = filter (\o -> not (oppIsWicPick o && oppId o == targetId)) opps
          wicOnly = filter oppIsWicPick updated
      encodeFile wicJobsFile wicOnly
      putStrLn "[Admin] Job removed."
      return updated
 
loadWicJobs :: IO [Opportunity]
loadWicJobs = do
  exists <- doesFileExist wicJobsFile
  if not exists
    then return []
    else do
      result <- eitherDecodeFileStrict wicJobsFile
      case result of
        Left  _ -> return []
        Right opps -> return opps

-- Helpers
 
emptyQuery :: SearchQuery
emptyQuery = SearchQuery
  { queryKeyword = Nothing
  , queryTags = []
  , queryType = Nothing
  , queryRemote = Nothing
  }
 
parseTag :: String -> Maybe Tag
parseTag s = case map toLower s of
  "software" -> Just Software
  "research" -> Just Research
  "remote" -> Just Remote
  "inperson" -> Just InPerson
  "paid" -> Just Paid
  "unpaid" -> Just Unpaid
  "parttime" -> Just PartTime
  "fulltime" -> Just FullTime
  _ -> Nothing
 
parseTags :: String -> [Tag]
parseTags s =
  let parts = splitOn ',' s
  in [t | Just t <- map (parseTag . trim) parts]
 
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn c (x:xs)
  | x == c = "" : splitOn c xs
  | otherwise = let (w:ws) = splitOn c xs in (x:w) : ws
 
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')