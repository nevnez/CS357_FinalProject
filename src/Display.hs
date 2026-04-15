-- UI/Display.hs
-- Terminal display helpers.
-- Assigned to: Kiana, Gael
--
-- TODO: replace the plain putStrLn calls with Layoutz widgets
--       for a polished TUI (boxes, colours, scrollable lists).

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

-- ── Individual opportunity card ────────────────────────────────────────────

displayOpportunity :: Day -> Opportunity -> IO ()
displayOpportunity today opp = do
  putStrLn $ replicate 60 '─'
  putStrLn $ "  " ++ oppTitle opp ++ "  @  " ++ oppCompany opp
  putStrLn $ "  Source : " ++ oppSource opp
  putStrLn $ "  Type   : " ++ show (oppType opp)
  putStrLn $ "  Tags   : " ++ unwords (map show (oppTags opp))
  putStrLn $ "  " ++ displayDeadlineBadge (deadlineStatus today opp) opp
  putStrLn $ "  URL    : " ++ oppURL opp
  putStrLn $ replicate 60 '─'

-- ── List of opportunities ──────────────────────────────────────────────────

displayList :: Day -> [Opportunity] -> IO ()
displayList _     []   = putStrLn "(No results found.)"
displayList today opps = mapM_ (displayOpportunity today) opps

-- ── Deadline badge ─────────────────────────────────────────────────────────

displayDeadlineBadge :: DeadlineStatus -> Opportunity -> String
displayDeadlineBadge Rolling  _   = "Deadline: Rolling / Open"
displayDeadlineBadge status   opp =
  let label = case status of
                Urgent  -> "[!! URGENT]"
                Soon    -> "[Soon]"
                Future  -> "[Future]"
  in "Deadline: " ++ maybe "N/A" show (oppDeadline opp) ++ " " ++ label

-- ── Main menu ──────────────────────────────────────────────────────────────

displayMenu :: IO ()
displayMenu = do
  putStrLn ""
  putStrLn "╔══════════════════════════════════════╗"
  putStrLn "║  WiC Opportunities CLI               ║"
  putStrLn "║  UNM Women in Computing              ║"
  putStrLn "╠══════════════════════════════════════╣"
  putStrLn "║  1. Browse all opportunities         ║"
  putStrLn "║  2. Search by keyword                ║"
  putStrLn "║  3. Filter by tag                    ║"
  putStrLn "║  4. View upcoming deadlines          ║"
  putStrLn "║  5. Recommendations for me           ║"
  putStrLn "║  6. Refresh (re-scrape)              ║"
  putStrLn "║  0. Quit                             ║"
  putStrLn "╚══════════════════════════════════════╝"
  putStr   "  Choose an option: "

-- ── Generic prompt ─────────────────────────────────────────────────────────

promptUser :: String -> IO String
promptUser msg = do
  putStr (msg ++ " ")
  getLine
