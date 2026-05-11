{-# LANGUAGE OverloadedStrings #-}

-- UI.hs  
-- Enhanced beautiful pink-themed terminal UI for WiC Opportunities
-- FIXED: Box alignment + removed upcoming deadlines option

module UI (runUI) where

import Types
import Scraper (loadFromCache, refreshAll)
import Search (search)
import Filter (filterByTags)
import Recommend (recommend, defaultProfile)
import Deadline (sortByDeadline, upcomingDeadlines, deadlineStatus, DeadlineStatus(..))
import Comments
import Preferences
import Data.Time (getCurrentTime, utctDay, Day)
import Data.Maybe (fromMaybe)
import Data.List (find, intercalate, isInfixOf, sortBy)
import Data.Char (toLower)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Prelude hiding (truncate)

-- ══════════════════════════════════════════════════════════════
-- ANSI COLOR CODES
-- ══════════════════════════════════════════════════════════════

pink, hotPink, purple, cyan, yellow, green, white, gray, red, bold, reset :: String
pink = "\ESC[38;5;219m"
hotPink = "\ESC[38;5;205m"
purple = "\ESC[38;5;141m"
cyan = "\ESC[38;5;117m"
yellow = "\ESC[38;5;228m"
green = "\ESC[38;5;120m"
white = "\ESC[97m"
gray = "\ESC[38;5;245m"
red = "\ESC[38;5;210m"
bold = "\ESC[1m"
reset = "\ESC[0m"

-- Emoji/Icons
star, heart, sparkle, check, arrow, rocket, mag, fire, tada :: String
star = "⭐"
heart = "💖"
sparkle = "✨"
check = "✓"
arrow = "→"
rocket = "🚀"
mag = "🔍"
fire = "🔥"
tada = "🎉"

admins :: [String]
admins = ["WICAdmin14", "nevnez14", "kiana14", "gael14"]

isAdmin :: String -> Bool
isAdmin name = map toLower name `elem` map (map toLower) admins

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
-- HELPER: Truncate long text properly
-- ══════════════════════════════════════════════════════════════

truncateStr :: Int -> String -> String
truncateStr n s = if length s > n then take (n - 3) s ++ "..." else s

-- Pad to exact width
padToWidth :: Int -> String -> String
padToWidth w s = 
  let len = length s
  in if len >= w 
     then take w s
     else s ++ replicate (w - len) ' '

-- ══════════════════════════════════════════════════════════════
-- ANIMATIONS & EFFECTS
-- ══════════════════════════════════════════════════════════════

showLoading :: String -> IO ()
showLoading msg = do
  let frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
  mapM_ (\frame -> do
    putStr $ "\r" ++ cyan ++ "  " ++ frame ++ " " ++ msg ++ "..." ++ reset
    hFlush stdout
    threadDelay 80000
    ) (take 10 $ cycle frames)
  putStr "\r"
  hFlush stdout

drawProgressBar :: Int -> Int -> Int -> String
drawProgressBar current total width =
  let percentage = if total == 0 then 0 else (current * 100) `div` total
      filled = (current * width) `div` max 1 total
      empty = width - filled
      bar = replicate filled '█' ++ replicate empty '░'
  in pink ++ "[" ++ bar ++ "] " ++ show percentage ++ "%" ++ reset

divider :: String -> String -> IO ()
divider color text = do
  let width = 60
      textLen = length text + 2
      sideLen = (width - textLen) `div` 2
  putStrLn $ color ++ replicate sideLen '─' ++ " " ++ text ++ " " ++ replicate sideLen '─' ++ reset

-- ══════════════════════════════════════════════════════════════
-- MAIN ENTRY POINT
-- ══════════════════════════════════════════════════════════════

runUI :: IO ()
runUI = do
  clearScreen
  drawAnimatedWelcome
  
  opps <- loadFromCache
  today <- utctDay <$> getCurrentTime
  cmap <- loadComments
  
  putStr $ pink ++ "\n  Enter your name or ADMIN key: " ++ reset
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
  
  clearScreen
  showStats opps today
  threadDelay 1500000
  
  mainLoop initialState

drawAnimatedWelcome :: IO ()
drawAnimatedWelcome = do
  putStrLn ""
  threadDelay 100000
  putStrLn $ hotPink ++ bold ++ "  ╔══════════════════════════════════════════╗" ++ reset
  threadDelay 100000
  putStrLn $ hotPink ++ bold ++ "  ║                                          ║" ++ reset
  threadDelay 100000
  putStrLn $ hotPink ++ bold ++ "  ║    WiC Opportunities Finder              ║" ++ reset
  threadDelay 100000
  putStrLn $ pink ++ "  ║      UNM Women in Computing              ║" ++ reset
  threadDelay 100000
  putStrLn $ pink ++ "  ║                                          ║" ++ reset
  threadDelay 100000
  putStrLn $ hotPink ++ bold ++ "  ╚══════════════════════════════════════════╝" ++ reset
  putStrLn ""

showStats :: [Opportunity] -> Day -> IO ()
showStats opps today = do
  let total = length opps
      wicPicks = length $ filter oppIsWicPick opps
      internships = length $ filter (\o -> oppType o == Internship) opps
      jobs = length $ filter (\o -> oppType o == Job) opps
      research = length $ filter (\o -> oppType o == ResearchPosition) opps
      sorted = sortByDeadline opps
      upcoming = upcomingDeadlines today 30 sorted
      urgent = length $ filter (\o -> deadlineStatus today o == Urgent) opps
  
  putStrLn $ cyan ++ bold ++ "\n  Quick Stats" ++ reset
  putStrLn ""
  
  putStrLn $ "  Total Opportunities: " ++ green ++ bold ++ show total ++ reset
  putStrLn $ "  " ++ drawProgressBar total total 30
  putStrLn ""
  
  putStrLn $ "  WiC Picks: " ++ yellow ++ bold ++ show wicPicks ++ reset
  putStrLn $ "  " ++ drawProgressBar wicPicks total 30
  putStrLn ""
  
  putStrLn $ "  Internships: " ++ pink ++ show internships ++ reset ++ 
             "  |  Jobs: " ++ purple ++ show jobs ++ reset ++
             "  |  Research: " ++ cyan ++ show research ++ reset
  putStrLn ""
  
  putStrLn $ "  Upcoming (30 days): " ++ green ++ show (length upcoming) ++ reset
  when (urgent > 0) $
    putStrLn $ "  " ++ red ++ bold ++ "URGENT deadlines: " ++ show urgent ++ reset
  
  putStrLn ""
  putStrLn $ gray ++ "  Loading..." ++ reset

-- ══════════════════════════════════════════════════════════════
-- MAIN LOOP
-- ══════════════════════════════════════════════════════════════

mainLoop :: AppState -> IO ()
mainLoop state = do
  clearScreen
  drawMainScreen state
  
  putStr $ pink ++ "\n  Enter command: " ++ reset
  hFlush stdout
  input <- getLine
  
  case map toLower input of
    "q" -> do
      clearScreen
      drawGoodbye
    
    "1" -> browseOpportunities (state { stateFiltered = stateOpps state, stateCurrentIndex = 0 })
    "2" -> searchPrompt state
    "3" -> filterPrompt state
    "4" -> do
      -- Load preferences and show recommendations
      pmap <- loadAllPrefs
      prefs <- case getPrefsFor (userName $ stateProfile state) pmap of
        Just p -> do
          putStrLn $ green ++ "\n  Using your saved preferences!" ++ reset
          return p
        Nothing -> do
          putStrLn $ yellow ++ "\n  No preferences saved yet! Let's set them up." ++ reset
          putStrLn ""
          p <- promptForPrefs
          _ <- savePrefsFor (userName $ stateProfile state) p pmap
          return p
      
      -- Score and filter opportunities
      let score opp = 
            length (filter (\kw -> kw `isInfixOf` map toLower (oppTitle opp ++ oppDescription opp)) (prefKeywords prefs))
            + length (filter (`elem` oppTags opp) (prefTags prefs))
          combined = filter (\o -> score o > 0) $
                     sortBy (\a b -> compare (score b) (score a)) (stateOpps state)
      
      showLoading "Calculating recommendations"
      putStrLn $ green ++ "\n  Found " ++ show (length combined) ++ " recommendations based on your preferences!" ++ reset
      threadDelay 500000
      
      browseOpportunities (state { stateFiltered = combined, stateCurrentIndex = 0 })
    "5" -> do
      clearScreen
      putStrLn $ cyan ++ "\n  Refreshing data from sources..." ++ reset
      putStrLn ""
      showLoading "Fetching opportunities"
      fresh <- refreshAll
      putStrLn $ green ++ bold ++ "\n  Success! Loaded " ++ show (length fresh) ++ " opportunities!" ++ reset
      pause
      mainLoop (state { stateOpps = fresh, stateFiltered = fresh })
    "c" -> commentsPrompt state
    "6" | stateIsAdmin state -> do
      putStrLn $ hotPink ++ "\n  [ADMIN] Add WiC Job - Coming soon!" ++ reset
      pause
      mainLoop state
    "7" | stateIsAdmin state -> do
      putStrLn $ hotPink ++ "\n  [ADMIN] Remove WiC Job - Coming soon!" ++ reset
      pause
      mainLoop state
    "s" -> do
      clearScreen
      showStats (stateOpps state) (stateToday state)
      pause
      mainLoop state
    _ -> do
      putStrLn $ red ++ "\n  Invalid option. Try again." ++ reset
      pause
      mainLoop state

drawGoodbye :: IO ()
drawGoodbye = do
  putStrLn ""
  putStrLn $ hotPink ++ bold ++ "  ╔══════════════════════════════════════════╗" ++ reset
  putStrLn $ hotPink ++ bold ++ "  ║                                          ║" ++ reset
  putStrLn $ hotPink ++ bold ++ "  ║       Thanks for using WiC!              ║" ++ reset
  putStrLn $ pink ++ "  ║                                          ║" ++ reset
  putStrLn $ pink ++ "  ║      Good luck with your search!         ║" ++ reset
  putStrLn $ hotPink ++ bold ++ "  ╚══════════════════════════════════════════╝" ++ reset
  putStrLn ""

-- ══════════════════════════════════════════════════════════════
-- PRETTY SCREENS
-- ══════════════════════════════════════════════════════════════

drawMainScreen :: AppState -> IO ()
drawMainScreen state = do
  let username = userName (stateProfile state)
      adminBadge = if stateIsAdmin state then " [ADMIN]" else ""
      filteredCount = length (stateFiltered state)
      totalCount = length (stateOpps state)
  
  putStrLn ""
  putStrLn $ hotPink ++ bold ++ "  ════════════════════════════════════════" ++ reset
  putStrLn $ pink ++ bold ++ "   Welcome, " ++ username ++ adminBadge ++ reset
  putStrLn $ hotPink ++ bold ++ "  ════════════════════════════════════════" ++ reset
  putStrLn ""
  
  let menuItems = if stateIsAdmin state
        then [ ("1", "Browse all opportunities")
             , ("2", "Search by keyword")
             , ("3", "Filter by tag")
             , ("4", "Recommendations for me")
             , ("5", "Refresh (fetch live data)")
             , ("C", "Comments (browse a job first!)")
             , ("S", "Show statistics")
             , ("6", "Add WiC curated job [ADMIN]")
             , ("7", "Remove WiC curated job [ADMIN]")
             , ("Q", "Quit")
             ]
        else [ ("1", "Browse all opportunities")
             , ("2", "Search by keyword")
             , ("3", "Filter by tag")
             , ("4", "Recommendations for me")
             , ("5", "Refresh (fetch live data)")
             , ("C", "Comments (browse a job first!)")
             , ("S", "Show statistics")
             , ("Q", "Quit")
             ]
  
  putStrLn $ pink ++ "  ┌──────────────────────────────────────┐" ++ reset
  mapM_ (\(key, desc) -> do
    let line = key ++ " " ++ arrow ++ " " ++ desc
        padded = padToWidth 38 line
    putStrLn $ "  │ " ++ cyan ++ padded ++ reset ++ pink ++ "│" ++ reset
    ) menuItems
  putStrLn $ pink ++ "  └──────────────────────────────────────┘" ++ reset
  
  putStrLn ""
  putStrLn $ gray ++ "  Showing: " ++ reset ++ show filteredCount ++ gray ++ " / " ++ reset ++ show totalCount ++ gray ++ " opportunities" ++ reset
  when (filteredCount < totalCount) $
    putStrLn $ yellow ++ "  (Filtered view - select option 1 to see all)" ++ reset

-- ══════════════════════════════════════════════════════════════
-- BROWSE MODE
-- ══════════════════════════════════════════════════════════════

browseOpportunities :: AppState -> IO ()
browseOpportunities state = do
  if null (stateFiltered state)
    then do
      clearScreen
      putStrLn $ pink ++ bold ++ "\n  Browse Opportunities" ++ reset
      putStrLn ""
      putStrLn $ gray ++ "  No opportunities found." ++ reset
      putStrLn $ gray ++ "  Try adjusting your filters or search terms." ++ reset
      pause
      mainLoop state
    else browseLoop state

browseLoop :: AppState -> IO ()
browseLoop state = do
  let opps = stateFiltered state
      idx = stateCurrentIndex state
      currentOpp = if idx < length opps then Just (opps !! idx) else Nothing
  
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  Browse Opportunities " ++ reset
  putStrLn $ gray ++ "  " ++ drawProgressBar (idx + 1) (length opps) 40 ++ reset
  putStrLn ""
  
  case currentOpp of
    Nothing -> do
      putStrLn $ red ++ "  No opportunity to display." ++ reset
      pause
      mainLoop state
    
    Just opp -> do
      drawOpportunityCard (stateToday state) opp
      
      putStrLn ""
      divider cyan "Navigation"
      putStrLn $ cyan ++ "  [N]" ++ reset ++ " Next  |  " ++ cyan ++ "[P]" ++ reset ++ " Prev  |  " ++ yellow ++ "[C]" ++ reset ++ " Comments  |  " ++ gray ++ "[B]" ++ reset ++ " Back"
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
-- OPPORTUNITY CARD (FIXED ALIGNMENT!)
-- ══════════════════════════════════════════════════════════════

drawOpportunityCard :: Day -> Opportunity -> IO ()
drawOpportunityCard today opp = do
  let boxWidth = 60
      contentWidth = boxWidth - 4
  
  -- Top border
  putStrLn $ pink ++ "  ╔" ++ replicate (boxWidth - 4) '═' ++ "╗" ++ reset
  
  -- Title line (TRUNCATE if too long)
  let titleBase = oppTitle opp ++ " @ " ++ oppCompany opp
      wicBadge = if oppIsWicPick opp then " [WiC]" else ""
      titleFull = titleBase ++ wicBadge
      titleLine = truncateStr contentWidth titleFull
  putStrLn $ "  ║ " ++ hotPink ++ bold ++ padToWidth contentWidth titleLine ++ reset ++ pink ++ "║" ++ reset
  
  -- Divider
  putStrLn $ pink ++ "  ╠" ++ replicate (boxWidth - 4) '─' ++ "╣" ++ reset
  
  -- Type (TRUNCATE if too long)
  let typeLine = truncateStr contentWidth ("Type: " ++ show (oppType opp))
  putStrLn $ "  ║ " ++ cyan ++ padToWidth contentWidth typeLine ++ reset ++ pink ++ "║" ++ reset
  
  -- Source (TRUNCATE if too long)
  let sourceLine = truncateStr contentWidth ("Source: " ++ oppSource opp)
  putStrLn $ "  ║ " ++ cyan ++ padToWidth contentWidth sourceLine ++ reset ++ pink ++ "║" ++ reset
  
  -- Tags (TRUNCATE if too long)
  let tagsLine = truncateStr contentWidth ("Tags: " ++ intercalate ", " (map show $ oppTags opp))
  putStrLn $ "  ║ " ++ purple ++ padToWidth contentWidth tagsLine ++ reset ++ pink ++ "║" ++ reset
  
  -- Divider
  putStrLn $ pink ++ "  ╠" ++ replicate (boxWidth - 4) '─' ++ "╣" ++ reset
  
  -- Deadline (TRUNCATE if too long)
  let status = deadlineStatus today opp
      (statusColor, statusText) = case status of
        Urgent  -> (red, "URGENT")
        Soon    -> (yellow, "Soon")
        Future  -> (green, "Future")
        Rolling -> (gray, "Rolling")
      deadlineFull = "Deadline: " ++ maybe "N/A" show (oppDeadline opp) ++ " [" ++ statusText ++ "]"
      deadlineLine = truncateStr contentWidth deadlineFull
  putStrLn $ "  ║ " ++ statusColor ++ bold ++ padToWidth contentWidth deadlineLine ++ reset ++ pink ++ "║" ++ reset
  
  -- Divider
  putStrLn $ pink ++ "  ╠" ++ replicate (boxWidth - 4) '─' ++ "╣" ++ reset
  
  -- Description header
  putStrLn $ "  ║ " ++ bold ++ padToWidth contentWidth "Description:" ++ reset ++ pink ++ "║" ++ reset
  
  -- Description lines (WRAP and TRUNCATE properly)
  let descLines = wrapText (contentWidth - 2) (oppDescription opp)
  mapM_ (\line -> do
    let truncated = truncateStr (contentWidth - 1) line
    putStrLn $ "  ║  " ++ padToWidth (contentWidth - 1) truncated ++ pink ++ "║" ++ reset
    ) (take 4 descLines)
  
  when (length descLines > 4) $
    putStrLn $ "  ║  " ++ gray ++ padToWidth (contentWidth - 1) "..." ++ reset ++ pink ++ "║" ++ reset
  
  -- Divider
  putStrLn $ pink ++ "  ╠" ++ replicate (boxWidth - 4) '─' ++ "╣" ++ reset
  
  -- URL (TRUNCATE if too long)
  let urlLine = truncateStr contentWidth ("URL: " ++ oppURL opp)
  putStrLn $ "  ║ " ++ cyan ++ padToWidth contentWidth urlLine ++ reset ++ pink ++ "║" ++ reset
  
  -- Bottom border
  putStrLn $ pink ++ "  ╚" ++ replicate (boxWidth - 4) '═' ++ "╝" ++ reset

-- ══════════════════════════════════════════════════════════════
-- SEARCH & FILTER
-- ══════════════════════════════════════════════════════════════

searchPrompt :: AppState -> IO ()
searchPrompt state = do
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  Search Opportunities" ++ reset
  putStrLn ""
  putStr $ cyan ++ "  Enter keyword: " ++ reset
  hFlush stdout
  keyword <- getLine
  
  if null keyword
    then mainLoop state
    else do
      showLoading "Searching"
      let query = emptyQuery { queryKeyword = Just keyword }
          results = search query (stateOpps state)
          newState = state { stateFiltered = results, stateCurrentIndex = 0 }
      
      putStrLn $ green ++ "\n  Found " ++ show (length results) ++ " opportunities matching '" ++ keyword ++ "'" ++ reset
      pause
      
      if null results
        then mainLoop newState
        else browseOpportunities newState

filterPrompt :: AppState -> IO ()
filterPrompt state = do
  clearScreen
  putStrLn $ pink ++ bold ++ "\n  Filter by Tag" ++ reset
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
      
      putStrLn $ green ++ "\n  Filtered to " ++ show (length filtered) ++ " opportunities with tag: " ++ show tag ++ reset
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
  putStrLn $ pink ++ bold ++ "\n  View/Add Comments" ++ reset
  putStrLn ""
  putStrLn $ yellow ++ "  Tip: Press 'C' while browsing to comment on a job!" ++ reset
  putStrLn ""
  putStrLn $ gray ++ "  Or enter a job ID if you know it:" ++ reset
  putStr $ cyan ++ "  Job ID (or press Enter to go back): " ++ reset
  hFlush stdout
  idStr <- getLine
  
  if null idStr
    then mainLoop state
    else case reads idStr of
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
  putStrLn $ pink ++ bold ++ "\n  Comments: " ++ oppTitle opp ++ " @ " ++ oppCompany opp ++ reset
  divider pink ""
  putStrLn ""
  
  let comments = getCommentsFor (oppId opp) (stateComments state)
  
  if null comments
    then putStrLn $ gray ++ "  No comments yet. Be the first!" ++ reset
    else mapM_ drawComment comments
  
  putStrLn ""
  divider cyan "Actions"
  putStrLn $ cyan ++ "  [A]" ++ reset ++ " Add comment  |  " ++ gray ++ "[B]" ++ reset ++ " Back"
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
          putStrLn $ green ++ "\n  Comment added!" ++ reset
          pause
          viewComments (state { stateComments = newCmap }) opp
    
    _ -> return ()

drawComment :: Comment -> IO ()
drawComment comment = do
  putStrLn $ purple ++ "  User: " ++ commentUser comment ++ reset ++ gray ++ " | " ++ commentDate comment ++ reset
  let lines = wrapText 56 (commentText comment)
  mapM_ (\line -> putStrLn $ "     " ++ line) lines
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
    go [] acc = if null acc then [] else [unwords (reverse acc)]
    go (w:ws) [] = go ws [w]
    go (w:ws) acc
      | length (unwords (reverse (w:acc))) <= width = go ws (w:acc)
      | otherwise = unwords (reverse acc) : go (w:ws) []

emptyQuery :: SearchQuery
emptyQuery = SearchQuery
  { queryKeyword = Nothing
  , queryTags = []
  , queryType = Nothing
  , queryRemote = Nothing
  }
