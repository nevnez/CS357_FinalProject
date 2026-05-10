{-# LANGUAGE OverloadedStrings #-}

-- UI.hs  
-- Beautiful pink-themed terminal UI for WiC Opportunities
-- Uses ANSI color codes for pretty formatting

module UI (runUI) where

import Types
import Scraper (loadFromCache, refreshAll)
import Search (search)
import Filter (filterByTags)
import Recommend (recommend, defaultProfile)
import Deadline (sortByDeadline, upcomingDeadlines, deadlineStatus, DeadlineStatus(..))
import Comments
import Data.Time (getCurrentTime, utctDay, Day)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Char (toLower)
import System.IO (hFlush, stdout)
import Prelude hiding (truncate)

-- ══════════════════════════════════════════════════════════════
-- ANSI COLOR CODES - Make it pretty! 💖
-- ══════════════════════════════════════════════════════════════

-- Colors
pink :: String
pink = "\ESC[38;5;219m"

hotPink :: String  
hotPink = "\ESC[38;5;205m"

purple :: String
purple = "\ESC[38;5;141m"

cyan :: String
cyan = "\ESC[38;5;117m"

yellow :: String
yellow = "\ESC[38;5;228m"

green :: String
green = "\ESC[38;5;120m"

white :: String
white = "\ESC[97m"

gray :: String
gray = "\ESC[38;5;245m"

red :: String
red = "\ESC[38;5;210m"

bold :: String
bold = "\ESC[1m"

reset :: String
reset = "\ESC[0m"

-- Emoji/Icons
star :: String
star = "⭐"

heart :: String
heart = "💖"

sparkle :: String
sparkle = "✨"

check :: String
check = "✓"

arrow :: String
arrow = "→"

-- Admin list
admins :: [String]
admins = ["WICAdmin14", "nevnez14", "kiana14", "gael14"]

isAdmin :: String -> Bool
isAdmin name = map toLower name `elem` map (map toLower) admins

-- Application State
data AppState = AppState
  { stateOpps :: [Opportunity]
  , stateFiltered :: [Opportunity]
  , stateProfile :: UserProfile
  , stateComments :: CommentMap
  , stateToday :: Day
  , stateIsAdmin :: Bool
  , stateCurrentIndex :: Int
  }

-- ══════════════════════════════════════════════════════════════
-- MAIN ENTRY POINT
-- ══════════════════════════════════════════════════════════════

runUI :: IO ()
runUI = do
  opps <- loadFromCache
  today <- utctDay <$> getCurrentTime
  cmap <- loadComments
  
  clearScreen
  drawWelcomeScreen
  
  putStr $ pink ++ "  Enter your name or ADMIN key: " ++ reset
  hFlush stdout
  name <- getLine
  
  let isAdminUser = isAdmin name
      profile = defaultProfile name
      initialState = AppState
        { stateOpps = opps
        , stateFiltered = opps
        , stateProfile = profile
        , stateComments = cmap
        , stateToday = today
        , stateIsAdmin = isAdminUser
        , stateCurrentIndex = 0
        }
  
  mainLoop initialState

-- ══════════════════════════════════════════════════════════════
-- MAIN LOOP
-- ══════════════════════════════════════════════════════════════

mainLoop :: AppState -> IO ()
mainLoop state = do
  clearScreen
  drawMainScreen state
  
  putStr $ pink ++ "\n  " ++ sparkle ++ " Enter command: " ++ reset
  hFlush stdout
  input <- getLine
  
  case map toLower input of
    "q" -> do
      clearScreen
      putStrLn $ hotPink ++ bold ++ "\n  " ++ heart ++ " Goodbye! Thanks for using WiC Opportunities " ++ heart ++ reset
      putStrLn ""
    
    "1" -> browseOpportunities (state { stateFiltered = stateOpps state, stateCurrentIndex = 0 })
    "2" -> searchPrompt state
    "3" -> filterPrompt state
    "4" -> do
      let sorted = sortByDeadline (stateOpps state)
          upcoming = upcomingDeadlines (stateToday state) 30 sorted
      browseOpportunities (state { stateFiltered = upcoming, stateCurrentIndex = 0 })
    "5" -> do
      let recommended = recommend (stateProfile state) (stateOpps state)
      browseOpportunities (state { stateFiltered = recommended, stateCurrentIndex = 0 })
    "6" -> do
      clearScreen
      putStrLn $ cyan ++ "\n  " ++ sparkle ++ " Refreshing data..." ++ reset
      fresh <- refreshAll
      putStrLn $ green ++ "  " ++ check ++ " Data refreshed!" ++ reset
      pause
      mainLoop (state { stateOpps = fresh, stateFiltered = fresh })
    "c" -> commentsPrompt state
    "7" | stateIsAdmin state -> do
      putStrLn $ hotPink ++ "\n  [ADMIN] Add WiC Job - Coming soon!" ++ reset
      pause
      mainLoop state
    "8" | stateIsAdmin state -> do
      putStrLn $ hotPink ++ "\n  [ADMIN] Remove WiC Job - Coming soon!" ++ reset
      pause
      mainLoop state
    _ -> do
      putStrLn $ red ++ "\n  Invalid option. Try again." ++ reset
      pause
      mainLoop state

-- ══════════════════════════════════════════════════════════════
-- PRETTY SCREENS
-- ══════════════════════════════════════════════════════════════

drawWelcomeScreen :: IO ()
drawWelcomeScreen = do
  putStrLn ""
  putStrLn $ hotPink ++ bold ++ "  ╔══════════════════════════════════════════╗" ++ reset
  putStrLn $ hotPink ++ bold ++ "  ║                                          ║" ++ reset
  putStrLn $ hotPink ++ bold ++ "  ║  " ++ sparkle ++ "  WiC Opportunities Finder  " ++ sparkle ++ "     ║" ++ reset
  putStrLn $ pink ++ "  ║      UNM Women in Computing              ║" ++ reset
  putStrLn $ pink ++ "  ║                                          ║" ++ reset
  putStrLn $ hotPink ++ bold ++ "  ╚══════════════════════════════════════════╝" ++ reset
  putStrLn ""

drawMainScreen :: AppState -> IO ()
drawMainScreen state = do
  let username = userName (stateProfile state)
      adminBadge = if stateIsAdmin state then " " ++ star ++ " ADMIN" else ""
  
  putStrLn ""
  putStrLn $ hotPink ++ bold ++ "  ═══════════════════════════════════════════════" ++ reset
  putStrLn $ pink ++ bold ++ "   " ++ heart ++ "  Welcome, " ++ username ++ adminBadge ++ reset
  putStrLn $ hotPink ++ bold ++ "  ═══════════════════════════════════════════════" ++ reset
  putStrLn ""
  
  let menuItems = if stateIsAdmin state
        then [ (cyan, "1", "Browse all opportunities")
             , (cyan, "2", "Search by keyword")
             , (cyan, "3", "Filter by tag")
             , (purple, "4", "View upcoming deadlines")
             , (pink, "5", "Recommendations for me")
             , (green, "6", "Refresh (fetch live data)")
             , (yellow, "C", "Comments on job")
             , (hotPink, "7", star ++ " Add WiC curated job [ADMIN]")
             , (hotPink, "8", star ++ " Remove WiC curated job [ADMIN]")
             , (gray, "Q", "Quit")
             ]
        else [ (cyan, "1", "Browse all opportunities")
             , (cyan, "2", "Search by keyword")
             , (cyan, "3", "Filter by tag")
             , (purple, "4", "View upcoming deadlines")
             , (pink, "5", "Recommendations for me")
             , (green, "6", "Refresh (fetch live data)")
             , (yellow, "C", "Comments on a job")
             , (gray, "Q", "Quit")
             ]
  
  putStrLn $ pink ++ "  ┌────────────────────────────────────────────┐" ++ reset
  mapM_ (\(color, key, desc) -> do
    putStrLn $ "  │ " ++ color ++ bold ++ key ++ reset ++ " " ++ arrow ++ " " ++ desc ++ replicate (40 - length key - length desc) ' ' ++ pink ++ "│" ++ reset
    ) menuItems
  putStrLn $ pink ++ "  └────────────────────────────────────────────┘" ++ reset
  
  putStrLn ""
  putStrLn $ gray ++ "  Currently showing: " ++ show (length $ stateFiltered state) ++ " opportunities" ++ reset

-- ══════════════════════════════════════════════════════════════
-- BROWSE MODE
-- ══════════════════════════════════════════════════════════════

browseOpportunities :: AppState -> IO ()
browseOpportunities state = do
  if null (stateFiltered state)
    then do
      clearScreen
      putStrLn $ pink ++ bold ++ "\n  " ++ sparkle ++ " Browse Opportunities " ++ sparkle ++ reset
      putStrLn $ gray ++ "\n  No opportunities found." ++ reset
      pause
      mainLoop state
    else browseLoop state

browseLoop :: AppState -> IO ()
browseLoop state = do
  let opps = stateFiltered state
      idx = stateCurrentIndex state
      currentOpp = if idx < length opps then Just (opps !! idx) else Nothing
  
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  " ++ sparkle ++ " Browse Opportunities (" ++ show (idx + 1) ++ "/" ++ show (length opps) ++ ") " ++ sparkle ++ reset
  putStrLn ""
  
  case currentOpp of
    Nothing -> do
      putStrLn $ red ++ "  No opportunity to display." ++ reset
      pause
      mainLoop state
    
    Just opp -> do
      drawOpportunityCard (stateToday state) opp
      
      putStrLn ""
      putStrLn $ cyan ++ "  [N]" ++ reset ++ " Next  │  " ++ cyan ++ "[P]" ++ reset ++ " Prev  │  " ++ yellow ++ "[C]" ++ reset ++ " Comments  │  " ++ gray ++ "[B]" ++ reset ++ " Back"
      putStr $ pink ++ "  " ++ arrow ++ " " ++ reset
      hFlush stdout
      
      input <- getLine
      case map toLower input of
        "n" -> browseLoop (state { stateCurrentIndex = min (length opps - 1) (idx + 1) })
        "p" -> browseLoop (state { stateCurrentIndex = max 0 (idx - 1) })
        "c" -> do
          viewComments state opp
          browseLoop state
        "b" -> mainLoop state
        _ -> browseLoop state

-- ══════════════════════════════════════════════════════════════
-- OPPORTUNITY CARD
-- ══════════════════════════════════════════════════════════════

drawOpportunityCard :: Day -> Opportunity -> IO ()
drawOpportunityCard today opp = do
  let width = 70
      wicBadge = if oppIsWicPick opp then " " ++ star ++ " WiC Pick" else ""
  
  putStrLn $ pink ++ "  ╔" ++ replicate (width - 4) '═' ++ "╗" ++ reset
  
  let titleLine = oppTitle opp ++ " @ " ++ oppCompany opp ++ wicBadge
  putStrLn $ "  ║ " ++ hotPink ++ bold ++ truncateStr (width - 6) titleLine ++ reset ++ replicate (max 0 (width - 6 - length titleLine)) ' ' ++ pink ++ "║" ++ reset
  
  putStrLn $ pink ++ "  ╠" ++ replicate (width - 4) '─' ++ "╣" ++ reset
  
  putStrLn $ "  ║ " ++ cyan ++ "Type:   " ++ reset ++ show (oppType opp) ++ replicate (max 0 (width - 18 - length (show $ oppType opp))) ' ' ++ pink ++ "║" ++ reset
  putStrLn $ "  ║ " ++ cyan ++ "Source: " ++ reset ++ oppSource opp ++ replicate (max 0 (width - 18 - length (oppSource opp))) ' ' ++ pink ++ "║" ++ reset
  
  let tagsStr = unwords (map show $ oppTags opp)
  putStrLn $ "  ║ " ++ purple ++ "Tags:   " ++ reset ++ truncateStr (width - 18) tagsStr ++ replicate (max 0 (width - 18 - length tagsStr)) ' ' ++ pink ++ "║" ++ reset
  
  putStrLn $ pink ++ "  ╠" ++ replicate (width - 4) '─' ++ "╣" ++ reset
  
  let status = deadlineStatus today opp
      (statusColor, statusSymbol, statusText) = case status of
        Urgent  -> (red, "⚠ ", "URGENT")
        Soon    -> (yellow, "⏰ ", "Soon")
        Future  -> (green, "📅 ", "Future")
        Rolling -> (gray, "🔄 ", "Rolling")
      deadlineStr = statusSymbol ++ "Deadline: " ++ maybe "N/A" show (oppDeadline opp) ++ " [" ++ statusText ++ "]"
  
  putStrLn $ "  ║ " ++ statusColor ++ deadlineStr ++ reset ++ replicate (max 0 (width - 6 - length deadlineStr + 6)) ' ' ++ pink ++ "║" ++ reset
  
  putStrLn $ pink ++ "  ╠" ++ replicate (width - 4) '─' ++ "╣" ++ reset
  
  putStrLn $ "  ║ " ++ bold ++ "Description:" ++ reset ++ replicate (width - 19) ' ' ++ pink ++ "║" ++ reset
  let descLines = wrapText (width - 8) (oppDescription opp)
  mapM_ (\line -> putStrLn $ "  ║   " ++ truncateStr (width - 8) line ++ replicate (max 0 (width - 8 - length line)) ' ' ++ pink ++ "║" ++ reset) (take 5 descLines)
  
  putStrLn $ pink ++ "  ╠" ++ replicate (width - 4) '─' ++ "╣" ++ reset
  
  let urlStr = "🔗 " ++ oppURL opp
  putStrLn $ "  ║ " ++ cyan ++ truncateStr (width - 6) urlStr ++ reset ++ replicate (max 0 (width - 6 - length urlStr)) ' ' ++ pink ++ "║" ++ reset
  
  putStrLn $ pink ++ "  ╚" ++ replicate (width - 4) '═' ++ "╝" ++ reset

-- ══════════════════════════════════════════════════════════════
-- SEARCH & FILTER
-- ══════════════════════════════════════════════════════════════

searchPrompt :: AppState -> IO ()
searchPrompt state = do
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  " ++ sparkle ++ " Search Opportunities " ++ sparkle ++ reset
  putStrLn ""
  putStr $ cyan ++ "  Enter keyword: " ++ reset
  hFlush stdout
  keyword <- getLine
  
  if null keyword
    then mainLoop state
    else do
      let query = emptyQuery { queryKeyword = Just keyword }
          results = search query (stateOpps state)
          newState = state { stateFiltered = results, stateCurrentIndex = 0 }
      
      putStrLn $ green ++ "\n  " ++ check ++ " Found " ++ show (length results) ++ " opportunities." ++ reset
      pause
      
      if null results
        then mainLoop newState
        else browseOpportunities newState

filterPrompt :: AppState -> IO ()
filterPrompt state = do
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  " ++ sparkle ++ " Filter by Tag " ++ sparkle ++ reset
  putStrLn ""
  let allTags = [minBound .. maxBound] :: [Tag]
  mapM_ (\(i, tag) -> putStrLn $ "  " ++ purple ++ show i ++ reset ++ ". " ++ show tag) (zip [1..] allTags)
  
  putStrLn ""
  putStr $ cyan ++ "  Enter tag number (or 0 to cancel): " ++ reset
  hFlush stdout
  input <- getLine
  
  case reads input :: [(Int, String)] of
    [(n, "")] | n > 0 && n <= length allTags -> do
      let tag = allTags !! (n - 1)
          filtered = filterByTags [tag] (stateOpps state)
          newState = state { stateFiltered = filtered, stateCurrentIndex = 0 }
      
      putStrLn $ green ++ "\n  " ++ check ++ " Filtered to " ++ show (length filtered) ++ " opportunities with tag: " ++ show tag ++ reset
      pause
      
      if null filtered
        then mainLoop newState
        else browseOpportunities newState
    
    _ -> mainLoop state

-- ══════════════════════════════════════════════════════════════
-- COMMENTS
-- ══════════════════════════════════════════════════════════════

commentsPrompt :: AppState -> IO ()
commentsPrompt state = do
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  " ++ sparkle ++ " View/Add Comments " ++ sparkle ++ reset
  putStrLn ""
  putStr $ cyan ++ "  Enter job ID: " ++ reset
  hFlush stdout
  idStr <- getLine
  
  case reads idStr of
    [(jobId, "")] ->
      case find (\o -> oppId o == jobId) (stateOpps state) of
        Nothing -> do
          putStrLn $ red ++ "\n  No job found with ID " ++ idStr ++ reset
          pause
          mainLoop state
        Just opp -> do
          viewComments state opp
          mainLoop state
    _ -> do
      putStrLn $ red ++ "\n  Invalid ID." ++ reset
      pause
      mainLoop state

viewComments :: AppState -> Opportunity -> IO ()
viewComments state opp = do
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  💬 Comments: " ++ oppTitle opp ++ " @ " ++ oppCompany opp ++ reset
  putStrLn $ pink ++ "  " ++ replicate 60 '─' ++ reset
  putStrLn ""
  
  let comments = getCommentsFor (oppId opp) (stateComments state)
  
  if null comments
    then putStrLn $ gray ++ "  No comments yet. Be the first!" ++ reset
    else mapM_ drawComment comments
  
  putStrLn ""
  putStrLn $ cyan ++ "  [A]" ++ reset ++ " Add comment  │  " ++ gray ++ "[B]" ++ reset ++ " Back"
  putStr $ pink ++ "  " ++ arrow ++ " " ++ reset
  hFlush stdout
  
  action <- getLine
  case map toLower action of
    "a" -> do
      putStrLn ""
      putStr $ cyan ++ "  Your comment: " ++ reset
      hFlush stdout
      commentText <- getLine
      
      if null commentText
        then return ()
        else do
          newCmap <- addComment (oppId opp) (userName $ stateProfile state) commentText (stateComments state)
          putStrLn $ green ++ "\n  " ++ check ++ " Comment added!" ++ reset
          pause
          viewComments (state { stateComments = newCmap }) opp
    
    _ -> return ()

drawComment :: Comment -> IO ()
drawComment comment = do
  putStrLn $ purple ++ "  👤 " ++ commentUser comment ++ reset ++ gray ++ " • " ++ commentDate comment ++ reset
  putStrLn $ "     " ++ commentText comment
  putStrLn ""

-- ══════════════════════════════════════════════════════════════
-- HELPER FUNCTIONS
-- ══════════════════════════════════════════════════════════════

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

pause :: IO ()
pause = do
  putStr $ gray ++ "\n  Press Enter to continue..." ++ reset
  hFlush stdout
  _ <- getLine
  return ()

wrapText :: Int -> String -> [String]
wrapText width text = go (words text) []
  where
    go [] acc = [unwords (reverse acc)]
    go (w:ws) [] = go ws [w]
    go (w:ws) acc
      | length (unwords (reverse (w:acc))) <= width = go ws (w:acc)
      | otherwise = unwords (reverse acc) : go (w:ws) []

truncateStr :: Int -> String -> String
truncateStr n s = if length s > n then take (n - 3) s ++ "..." else s

emptyQuery :: SearchQuery
emptyQuery = SearchQuery
  { queryKeyword = Nothing
  , queryTags = []
  , queryType = Nothing
  , queryRemote = Nothing
  }
