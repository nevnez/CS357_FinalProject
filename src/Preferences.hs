{-# LANGUAGE OverloadedStrings #-}

module Preferences
    (UserPrefs(..)
    , loadAllPrefs
    , saveAllPrefs
    , getPrefsFor
    , savePrefsFor
    , promptForPrefs
    ) where

import Types
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Data.Char (toLower)

-- Types

data UserPrefs = UserPrefs
    { prefKeywords :: [String]
    , prefTags :: [Tag]
    } deriving (Show, Eq)

instance FromJSON UserPrefs where
    parseJSON = withObject "UserPrefs" $ \v ->
        UserPrefs
            <$> v .: "keywords"
            <*> v .: "tags"

instance ToJSON UserPrefs where
    toJSON p = object
        ["keywords" .= prefKeywords p
        , "tags" .= prefTags p
        ]

type PrefsMap = Map String UserPrefs

prefsFile :: FilePath
prefsFile = "preferences.json"

-- Load and save

loadAllPrefs :: IO PrefsMap
loadAllPrefs = do
    exists <- doesFileExist prefsFile
    if not exists
        then return Map.empty
        else do
            result <- eitherDecodeFileStrict prefsFile
            case result of
                Left _ -> return Map.empty
                Right m -> return m

saveAllPrefs :: PrefsMap -> IO()
saveAllPrefs m = encodeFile prefsFile m 

-- Helpers

getPrefsFor :: String -> PrefsMap -> Maybe UserPrefs
getPrefsFor username pmap = Map.lookup (username :: String) pmap

savePrefsFor :: String -> UserPrefs -> PrefsMap -> IO PrefsMap
savePrefsFor username prefs pmap = do
    let updated = Map.insert username prefs pmap
    saveAllPrefs updated
    return updated

-- Prompts user to enter their preferences

promptForPrefs :: IO UserPrefs
promptForPrefs = do
    putStrLn "\n What are you looking for? \n"
    putStr "  Keywords (comma separated, e.g. python, backend, ML):\n> "
    hFlush stdout
    kwLine <- getLine
    let keywords = splitOn ',' kwLine

    putStrLn "\n Tags: Software, Research, Remote, InPerson, Paid, Unpaid, PartTime, FullTime"
    putStr "Tags (seperated by comma): \n> "
    hFlush stdout
    tagLine <- getLine
    let tags = parseTags tagLine
    return (UserPrefs keywords tags)

-- Helpers

parseTags :: String -> [Tag]
parseTags s = 
    let parts = splitOn ',' s
    in [t | Just t <- map (parseTag . trim) parts]

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

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn c (x:xs)
    | x == c = "" : splitOn c xs
    | otherwise = let (w:ws) = splitOn c xs in (x:w) : ws    

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile (== ' ')
    
