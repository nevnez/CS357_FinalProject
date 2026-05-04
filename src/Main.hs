-- app/Main.hs
-- Entry point: main loop wiring the scraper, backend, and UI together.

module Main where

import Types
import Scraper(scrapeAll)
import Search (search)
import Types
import Filter(filterByTags, filterPaid, filterRemote)
import Recommend(recommend, defaultProfile)
import Deadline(sortByDeadline, upcomingDeadlines)
import Display

import Data.Time (getCurrentTime, utctDay, Day)

main :: IO ()
main = do
  putStrLn "Fetching opportunities..."
  opps <- scrapeAll
  today <- utctDay <$> getCurrentTime

  -- Use a default profile until real user profiles are implemented.
  let profile = defaultProfile "Guest"

  loop today opps profile

-- Main loop.
loop :: Day -> [Opportunity] -> UserProfile -> IO ()
loop today opps profile = do
  displayMenu
  choice <- getLine
  case choice of
    "1" -> do
      displayList today opps
      loop today opps profile

    "2" -> do
      kw <- promptUser "Enter keyword:"
      let results = search (emptyQuery { queryKeyword = Just kw }) opps
      displayList today results
      loop today opps profile

    "3" -> do
      putStrLn "Available tags: Software, Research, Remote, InPerson, Paid, Unpaid, PartTime, FullTime"
      tagStr <- promptUser "Enter tag (e.g. Remote):"
      -- TODO: parse tagStr into a Tag value properly
      putStrLn ("(Tag filtering for '" ++ tagStr ++ "' coming soon!)")
      loop today opps profile

    "4" -> do
      daysStr <- promptUser "Show deadlines within how many days? (default 30):"
      let n = if null daysStr then 30 else read daysStr
      let upcoming = upcomingDeadlines today n (sortByDeadline opps)
      displayList today upcoming
      loop today opps profile

    "5" -> do
      let ranked = recommend profile opps
      displayList today ranked
      loop today opps profile

    "6" -> do
      putStrLn "Re-scraping..."
      fresh <- scrapeAll
      loop today fresh profile

    "0" -> putStrLn "Goodbye!"

    _   -> do
      putStrLn "Invalid option, try again."
      loop today opps profile

-- | A blank query (matches everything).
emptyQuery :: SearchQuery
emptyQuery = SearchQuery
  { queryKeyword = Nothing
  , queryTags    = []
  , queryType    = Nothing
  , queryRemote  = Nothing
  }
