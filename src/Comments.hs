-- {-# LANGUAGE OverloadedStrings #-}
-- Comments.hs
-- Stores and retrieves user comments on job listings.
-- Comments are saved to comments.json as: { "jobId": [{ "user": "...", "text": "...", "date": "..." }]}

module Comments
    (Comments(..)
    , loadComments
    , saveComments
    , getCommentsFor
    , addComment
    , displayComments
    ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import Data.Map (Map)
import Data.Time (getCurrentTime, utctDay, Day)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)  
import Text.Read (Lexeme(Ident))

-- Types

data Comment = Comment
    { commentUser :: String
    , commentText :: String
    , commentDate :: String
    } deriving (Show, Eq)

instance FromJSON Comment where
    parseJSON = withObject "Comment" $ \v ->
        Comment
            <$> v .: "user"
            <*> v .: "text"
            <*> v .: "date"

instance ToJSON Comment where
    toJSON c = object
        ["User" .= commentUser c, "text" .= commentText c, "date" .= commentDate c]            

-- Comments are stored as a map from job ID (String) to list of comments
type CommentMap = Map String [Comment]

commentsFile :: FilePath
commentsFile = "comments.json"

-- Load & Save 
loadComments :: IO CommentMap
loadComments = do 
    exist <- doesFileExist commentsFile
    if not exist
        then return Map.empty
        else do
            result <- eitherDecodeFileStrict commentsFile
            case result of 
                Left _ -> return Map.empty
                Right cmap -> return cmap

saveComments :: CommentMap -> IO ()
saveComments cmap = encodeFile commentsFile cmap

-- Helpers

-- Get all comments for a specific job ID
getCommentsFor :: Int -> CommentMap -> [Comment]
getCommentsFor jobId cmap = 
    Map.findWithDefault [] (show jobId) concatMap

-- Add comment to a job and save to disk
addComment :: Int -> String -> String -> CommentMap -> IO CommentMap
addComment jobId userName text cmap = do
    today <- show . utctDay <$> getCurrentTime
    let newComment = Comment
        {commentUser = userName, commentText = text, commentDate = today}
        key = show jobId
        existing = Map.findWithDefault [] key cmap 
        updated = Map.insert key (existing ++ [newComment]) cmap saveComments updated 
        return updated   

-- Display
displayComments :: [Comment] -> IO ()
-- TODO: Fix formatting
displayComments [] = putStrLn "No comments yet. Be the first!"
displayComments cs = mapM_displayOne cs
    where
        displayOne c = do
            putStrLn $ "  ┌ " ++ commentUser c ++ " (" ++ commentDate c ++ ")"
            putStrLn $ "  └ " ++ commentText c
            putStrLn ""