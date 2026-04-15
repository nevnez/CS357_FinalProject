-- Backend/Deadline.hs
-- Deadline tracking and sorting.
-- Uses the `time` library.
-- Assigned to: Nevaeh, Gael

module Deadline
  ( sortByDeadline
  , upcomingDeadlines
  , deadlineStatus
  , DeadlineStatus(..)
  ) where

import Types
import Data.Time (Day, diffDays)
import Data.List (sortBy)
import Data.Ord  (comparing)
import Data.Maybe (mapMaybe)

-- | How urgent a deadline is.
data DeadlineStatus
  = Urgent    -- within 7 days
  | Soon      -- within 30 days
  | Future    -- more than 30 days away
  | Rolling   -- no deadline set
  deriving (Show, Eq)

-- | Sort opportunities by deadline, soonest first.
-- Opportunities with no deadline (rolling) go to the end.
sortByDeadline :: [Opportunity] -> [Opportunity]
sortByDeadline = sortBy (comparing deadlineSortKey)
  where
    deadlineSortKey opp = case oppDeadline opp of
      Nothing  -> maxBound :: Int   -- rolling → last
      Just day -> fromIntegral (diffDays day (read "1900-01-01"))

-- | Return only opportunities with a deadline within `n` days from today.
upcomingDeadlines :: Day -> Int -> [Opportunity] -> [Opportunity]
upcomingDeadlines today n opps =
  [ opp
  | opp <- opps
  , Just deadline <- [oppDeadline opp]
  , let daysLeft = diffDays deadline today
  , daysLeft >= 0 && daysLeft <= fromIntegral n
  ]

-- | Classify how urgent an opportunity's deadline is.
deadlineStatus :: Day -> Opportunity -> DeadlineStatus
deadlineStatus today opp =
  case oppDeadline opp of
    Nothing      -> Rolling
    Just deadline ->
      let daysLeft = diffDays deadline today
      in if daysLeft <= 7  then Urgent
         else if daysLeft <= 30 then Soon
         else Future
