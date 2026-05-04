-- Display.hs
-- Terminal display helpers.

module Display
  ( displayOpportunity
  , displayList
  , displayDeadlineBadge
  , displayMenu
  , promptUser
  ) where

import Types
import Deadline (DeadlineStatus(..), deadlineStatus)
import Data.Time (Day)

-- Individual opportunity card 

displayOpportunity :: Day -> Opportunity -> IO ()
displayOpportunity today opp = do
  putStrLn $ replicate 60 '─'
  let wicBadge = if oppIsWicPick opp then "  [*] WiC Pick!" else ""
  putStrLn $ "  " ++ oppTitle opp ++ "  @  " ++ oppCompany opp ++ wicBadge
  putStrLn $ "  Source : " ++ oppSource opp
  putStrLn $ "  Type   : " ++ show (oppType opp)
  putStrLn $ "  Tags   : " ++ unwords (map show (oppTags opp))
  putStrLn $ "  " ++ displayDeadlineBadge (deadlineStatus today opp) opp
  putStrLn $ "  URL    : " ++ oppURL opp
  putStrLn $ replicate 60 '─'

-- List of opportunities 

displayList :: Day -> [Opportunity] -> IO ()
displayList _  [] = putStrLn "(No results found.)"
displayList today opps = do
  putStrLn ("Showing " ++ show (length opps) ++ " opportunities:\n")
  mapM_ (displayOpportunity today) opps

-- Deadline badge

displayDeadlineBadge :: DeadlineStatus -> Opportunity -> String
displayDeadlineBadge Rolling _  = "Deadline: Rolling / Open"
displayDeadlineBadge status opp =
  let label = case status of
                Urgent -> "[!! URGENT]"
                Soon -> "[Soon]"
                Future -> "[Future]"
  in "Deadline: " ++ maybe "N/A" show (oppDeadline opp) ++ " " ++ label

-- Main menu

displayMenu :: Bool -> IO ()
displayMenu isAdmin = do
  putStrLn ""
  if isAdmin
    then do
      putStrLn "╔══════════════════════════════════════╗"
      putStrLn "║  WiC Opportunities CLI  [ADMIN]      ║"
      putStrLn "╠══════════════════════════════════════╣"
      putStrLn "║  1. Browse all opportunities         ║"
      putStrLn "║  2. Search by keyword                ║"
      putStrLn "║  3. Filter by tag                    ║"
      putStrLn "║  4. View upcoming deadlines          ║"
      putStrLn "║  5. Recommendations for me           ║"
      putStrLn "║  6. Refresh (fetch live data)        ║"
      putStrLn "║  7. Add WiC curated job [ADMIN]      ║"
      putStrLn "║  8. Remove WiC curated job [ADMIN]   ║"
      putStrLn "║  0. Quit                             ║"
      putStrLn "╚══════════════════════════════════════╝"
      putStr "  Choose an option: "
    else do
      putStrLn "╔══════════════════════════════════════╗"
      putStrLn "║  WiC Opportunities CLI               ║"
      putStrLn "╠══════════════════════════════════════╣"
      putStrLn "║  1. Browse all opportunities         ║"
      putStrLn "║  2. Search by keyword                ║"
      putStrLn "║  3. Filter by tag                    ║"
      putStrLn "║  4. View upcoming deadlines          ║"
      putStrLn "║  5. Recommendations for me           ║"
      putStrLn "║  6. Refresh (fetch live data)        ║"
      putStrLn "║  0. Quit                             ║"
      putStrLn "╚══════════════════════════════════════╝"
      putStr "  Choose an option: "

-- Generic prompt

promptUser :: String -> IO String
promptUser msg = do
  putStr ("\n" ++ msg ++ "\n> ")
  getLine