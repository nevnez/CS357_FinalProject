-- Scraper/Scraper.hs
-- Live scraper for job/internship sites.
-- Assigned to: Kiana, Nevaeh
--
-- Uses:
--   http-conduit  - makes the HTTP request
--   scalpel       - parses the HTML response

module Scraper
  ( scrapeAll
  , scrapeLinkedIn
  , scrapeHandshake
  , scrapeIndeed
  ) where

import Types
import Data.Time (Day, fromGregorian)

-- | Scrape all configured sources and combine results.
-- TODO: run these concurrently with `async` for speed.
scrapeAll :: IO [Opportunity]
scrapeAll = do
  linkedIn  <- scrapeLinkedIn
  handshake <- scrapeHandshake
  indeed    <- scrapeIndeed
  return (linkedIn ++ handshake ++ indeed)

-- | Scrape LinkedIn for internships/jobs.
-- TODO: implement with http-conduit + scalpel.
--   1. Use simpleHttp to fetch the search URL
--   2. Use scrapeStringLike to extract title, company, tags, deadline
--   3. Map results into [Opportunity]
scrapeLinkedIn :: IO [Opportunity]
scrapeLinkedIn = do
  putStrLn "[Scraper] LinkedIn: TODO - replace with real scraper"
  return sampleOpportunities

-- | Scrape Handshake for student-focused opportunities.
-- TODO: Handshake requires login — consider using saved session cookies
--       passed in via environment variable.
scrapeHandshake :: IO [Opportunity]
scrapeHandshake = do
  putStrLn "[Scraper] Handshake: TODO - replace with real scraper"
  return []

-- | Scrape Indeed for general job listings.
-- TODO: implement with http-conduit + scalpel.
scrapeIndeed :: IO [Opportunity]
scrapeIndeed = do
  putStrLn "[Scraper] Indeed: TODO - replace with real scraper"
  return []

-- ---------------------------------------------------------------------------
-- Sample data for development/testing before the real scrapers are done
-- ---------------------------------------------------------------------------

sampleOpportunities :: [Opportunity]
sampleOpportunities =
  [ Opportunity
      { oppId          = 1
      , oppTitle       = "Software Engineering Intern"
      , oppCompany     = "Acme Corp"
      , oppDescription = "Build internal tooling with a great team."
      , oppTags        = [Software, Paid, Remote]
      , oppType        = Internship
      , oppDeadline    = Just (fromGregorian 2025 5 1)
      , oppURL         = "https://linkedin.com/jobs/1"
      , oppSource      = "LinkedIn"
      }
  , Opportunity
      { oppId          = 2
      , oppTitle       = "CS Research Assistant"
      , oppCompany     = "UNM CS Dept"
      , oppDescription = "Assist in HCI research, co-authorship possible."
      , oppTags        = [Research, InPerson, Paid]
      , oppType        = ResearchPosition
      , oppDeadline    = Just (fromGregorian 2025 4 15)
      , oppURL         = "https://cra.org/jobs/2"
      , oppSource      = "CRA"
      }
  , Opportunity
      { oppId          = 3
      , oppTitle       = "Backend Developer (Part-time)"
      , oppCompany     = "StartupXYZ"
      , oppDescription = "Haskell/Rust backend work, flexible hours."
      , oppTags        = [Software, Remote, Paid, PartTime]
      , oppType        = Job
      , oppDeadline    = Nothing
      , oppURL         = "https://wellfound.com/jobs/3"
      , oppSource      = "Wellfound"
      }
  ]
