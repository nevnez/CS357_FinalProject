--{-# LANGUAGE OverloadedStrings #-}
-- Database.hs
-- PostgreSQL persistence layer for WiC Opportunities CLI.
-- Replaces:  comments.json   → comments table
--            wic-jobs.json   → wic_opportunities table
--            in-memory UserProfile → users + user_tag_weights + user_favorites
 
module Database
  ( -- * Connection
    withDB
  , DBConn
 
    -- * Users
  , getOrCreateUser
  , setAdmin
 
    -- * Tag weights  (recommendation profile)
  , loadTagWeights
  , saveTagWeights
  , boostTagDB
 
    -- * Favorites
  , loadFavorites
  , addFavoriteDB
  , removeFavoriteDB
 
    -- * WiC-curated opportunities
  , loadWicOpportunities
  , insertWicOpportunity
  , deleteWicOpportunity
 
    -- * Comments
  , loadCommentsForJob
  , addCommentDB
  ) where
 
import Types
import Comments (Comment(..))
 
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types   (PGArray(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time  (getCurrentTime, utctDay)
import Data.Char  (toLower)
import Data.Maybe (fromMaybe)
import Control.Exception (bracket)
 
-- ---------------------------------------------------------------------------
-- Connection
-- ---------------------------------------------------------------------------
 
type DBConn = Connection
 
-- | Open a connection, run an action, close cleanly.
--   Reads credentials from environment or uses defaults for local dev.
withDB :: (DBConn -> IO a) -> IO a
withDB action = bracket open close action
  where
    open = connect defaultConnectInfo
      { connectHost     = "localhost"
      , connectPort     = 5432
      , connectDatabase = "wic"        -- create this DB in DataGrip first
      , connectUser     = "postgres"
      , connectPassword = "postgres"   -- change to your DataGrip password
      }
 
-- ---------------------------------------------------------------------------
-- Users
-- ---------------------------------------------------------------------------
 
-- | Look up a user by username; insert with default weights if new.
getOrCreateUser :: DBConn -> String -> IO Int
getOrCreateUser conn uname = do
  rows <- query conn
    "SELECT id FROM users WHERE username = ?"
    (Only uname) :: IO [Only Int]
  case rows of
    (Only uid : _) -> return uid
    [] -> do
      [Only uid] <- query conn
        "INSERT INTO users (username) VALUES (?) RETURNING id"
        (Only uname)
      -- Seed all tags with weight 1.0
      let allTags = [minBound..maxBound] :: [Tag]
      mapM_ (\t -> execute conn
        "INSERT INTO user_tag_weights (user_id, tag, weight) VALUES (?,?,?)"
        (uid, show t, 1.0 :: Double)) allTags
      return uid
 
-- | Grant or revoke admin for a username.
setAdmin :: DBConn -> String -> Bool -> IO ()
setAdmin conn uname flag =
  execute conn "UPDATE users SET is_admin = ? WHERE username = ?" (flag, uname)
  >> return ()
 
-- ---------------------------------------------------------------------------
-- Tag weights  (recommendation profile)
-- ---------------------------------------------------------------------------
 
-- | Load all tag weights for a user; missing tags default to 1.0.
loadTagWeights :: DBConn -> Int -> IO (Map Tag Double)
loadTagWeights conn uid = do
  rows <- query conn
    "SELECT tag, weight FROM user_tag_weights WHERE user_id = ?"
    (Only uid) :: IO [(String, Double)]
  let parsed = [ (readTag t, w) | (t, w) <- rows ]
      valid  = [ (tag, w) | Just (tag, w) <- map (\(mt,w) -> fmap (,w) mt) parsed ]
  return (Map.fromList valid)
 
-- | Persist the full tag weight map for a user (upsert).
saveTagWeights :: DBConn -> Int -> Map Tag Double -> IO ()
saveTagWeights conn uid weights =
  mapM_ upsertOne (Map.toList weights)
  where
    upsertOne (tag, w) = execute conn
      "INSERT INTO user_tag_weights (user_id, tag, weight) VALUES (?,?,?) \
      \ON CONFLICT (user_id, tag) DO UPDATE SET weight = EXCLUDED.weight"
      (uid, show tag, w)
 
-- | Increase one tag's weight by `delta` (used after favouriting).
boostTagDB :: DBConn -> Int -> Tag -> Double -> IO ()
boostTagDB conn uid tag delta = execute conn
  "INSERT INTO user_tag_weights (user_id, tag, weight) VALUES (?,?,?) \
  \ON CONFLICT (user_id, tag) DO UPDATE \
  \SET weight = user_tag_weights.weight + EXCLUDED.weight"
  (uid, show tag, delta)
  >> return ()
 
-- ---------------------------------------------------------------------------
-- Favorites
-- ---------------------------------------------------------------------------
 
-- | Return list of opportunity IDs the user has favourited.
loadFavorites :: DBConn -> Int -> IO [Int]
loadFavorites conn uid = do
  rows <- query conn
    "SELECT opp_id FROM user_favorites WHERE user_id = ? ORDER BY added_at"
    (Only uid) :: IO [Only Int]
  return (map fromOnly rows)
 
-- | Add a favourite (idempotent).
addFavoriteDB :: DBConn -> Int -> Int -> IO ()
addFavoriteDB conn uid oppId = execute conn
  "INSERT INTO user_favorites (user_id, opp_id) VALUES (?,?) \
  \ON CONFLICT DO NOTHING"
  (uid, oppId)
  >> return ()
 
-- | Remove a favourite.
removeFavoriteDB :: DBConn -> Int -> Int -> IO ()
removeFavoriteDB conn uid oppId = execute conn
  "DELETE FROM user_favorites WHERE user_id = ? AND opp_id = ?"
  (uid, oppId)
  >> return ()
 
-- ---------------------------------------------------------------------------
-- WiC-curated opportunities  (replaces wic-jobs.json)
-- ---------------------------------------------------------------------------
 
-- | Load all WiC-curated opportunities from the DB.
loadWicOpportunities :: DBConn -> IO [Opportunity]
loadWicOpportunities conn = do
  rows <- query_ conn
    "SELECT id, title, company, description, tags, \
    \       opp_type, deadline, url, source, is_wic_pick \
    \FROM   wic_opportunities \
    \ORDER  BY id"
  return (map rowToOpp rows)
  where
    rowToOpp (oid, title, company, desc, PGArray tags, otyp, deadline, url, src, isPick) =
      Opportunity
        { oppId          = oid
        , oppTitle       = title
        , oppCompany     = company
        , oppDescription = desc
        , oppTags        = [t | Just t <- map readTag tags]
        , oppType        = fromMaybe Job (readOppType otyp)
        , oppDeadline    = deadline
        , oppURL         = url
        , oppSource      = src
        , oppIsWicPick   = isPick
        }
 
-- | Insert a new WiC-curated opportunity; returns the assigned ID.
insertWicOpportunity :: DBConn -> Opportunity -> IO Int
insertWicOpportunity conn opp = do
  [Only newId] <- query conn
    "INSERT INTO wic_opportunities \
    \  (title, company, description, tags, opp_type, deadline, url, source, is_wic_pick) \
    \VALUES (?,?,?,?,?,?,?,?,?) \
    \RETURNING id"
    ( oppTitle opp
    , oppCompany opp
    , oppDescription opp
    , PGArray (map show (oppTags opp))
    , show (oppType opp)
    , oppDeadline opp
    , oppURL opp
    , oppSource opp
    , oppIsWicPick opp
    )
  return newId
 
-- | Delete a WiC-curated opportunity by its ID.
deleteWicOpportunity :: DBConn -> Int -> IO ()
deleteWicOpportunity conn oid = execute conn
  "DELETE FROM wic_opportunities WHERE id = ?"
  (Only oid)
  >> return ()
 
-- ---------------------------------------------------------------------------
-- Comments  (replaces comments.json / CommentMap)
-- ---------------------------------------------------------------------------
 
-- | Load all comments for a specific opportunity.
loadCommentsForJob :: DBConn -> Int -> IO [Comment]
loadCommentsForJob conn oid = do
  rows <- query conn
    "SELECT username, body, created_at::text \
    \FROM   comments \
    \WHERE  opp_id = ? \
    \ORDER  BY created_at, id"
    (Only oid) :: IO [(String, String, String)]
  return [Comment u b d | (u, b, d) <- rows]
 
-- | Persist a new comment to the DB.
addCommentDB :: DBConn -> Int -> String -> String -> IO ()
addCommentDB conn oid uname body = do
  today <- show . utctDay <$> getCurrentTime
  execute conn
    "INSERT INTO comments (opp_id, username, body, created_at) VALUES (?,?,?,?)"
    (oid, uname, body, today)
  return ()
 
-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------
 
readTag :: String -> Maybe Tag
readTag s = case s of
  "Software"   -> Just Software
  "Research"   -> Just Research
  "Remote"     -> Just Remote
  "InPerson"   -> Just InPerson
  "Paid"       -> Just Paid
  "Unpaid"     -> Just Unpaid
  "PartTime"   -> Just PartTime
  "FullTime"   -> Just FullTime
  _            -> Nothing
 
readOppType :: String -> Maybe OpportunityType
readOppType s = case s of
  "Internship"       -> Just Internship
  "Job"              -> Just Job
  "ResearchPosition" -> Just ResearchPosition
  _                  -> Nothing