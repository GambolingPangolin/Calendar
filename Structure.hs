{-|	Structures for the calendar parsing app

-}

module Structure (
  Entry(..)
  , Time(..)
  , Repeat(..)
  , today
  , isRepeating
  ) where

import Control.Monad (liftM)
import Data.Function ((&))
import Data.List (intercalate, concat)
import Data.Maybe (isJust, maybe)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day, showGregorian)

-- Preferred syntax for reverse function application
(|>) = (&)

{-| A calendar entry has the form
 -  STARTDATE [STARTTIME] [ENDDATE] [ENDTIME] BODY [(NOTE)] [REPEAT]

-}
data Entry = Entry
  { startDate :: Day
  , startTime :: Maybe Time
  , endDate :: Maybe Day
  , endTime :: Maybe Time
  , body :: String
  , note :: Maybe String
  , rep :: Maybe Repeat
  , subEntries :: [Entry]
  } deriving Eq

instance Ord Entry where
  e1 <= e2 =
    startDate e1 <= (startDate e2)

instance Show Entry where
  show e =
    let

      mss :: Show a => Maybe a -> [String]
      mss = maybe [] (return . show) 
      
      -- Format the sub-entries
      se :: String
      se = if length (subEntries e) == 0 
             then ""
             else 
               '\t' : intercalate "\t" (map show $ subEntries e)

    in
    [ [showGregorian (startDate e)]
    , mss (startTime e)
    , maybe [] (return . showGregorian) (endDate e) 
    , mss (endTime e)
    , [body e]
    , maybe [] (\ s -> ['(' : (s ++ ")")]) $ note e
    , mss (rep e)
    ]
    |> concat 
    |> intercalate " "
    |> flip (++) "\n"
    |> flip (++) se 

{-| Get the current date, formatted as a Day

 -}
today :: IO Day
today =
  liftM utctDay getCurrentTime

{-| We represent times as records containing an hour and a minute where the hour is 1 through 24.

-}
data Time = Time
  { hour :: Int
  , minute :: Int
  } deriving Eq

instance Ord Time where
  t1 <= t2 =
    hour t1 < hour t2
    || (hour t1 == hour t2 && minute t1 <= minute t2)

instance Show Time where
  show t =
    show' (hour t) ++ ":" ++ (show' $ minute t) 

{-| An event can repeat daily, weekly or monthly.
 -  TODO: Add a constructor for more complex repetition.

-}
data Repeat = 
  Daily 
  | Weekly 
  | Yearly
  deriving Eq

instance Show Repeat where
  show r =
    case r of 
      Daily   -> "^d"
      Weekly  -> "^w"
      Yearly  -> "^y"

{-| Check if an entry has a set repeat.

-}
isRepeating :: Entry -> Bool
isRepeating = isJust . rep

-- If the Int is less than 10, add a leading zero
show' :: Int -> String
show' i
  | 0 <= i && i < 10 = '0' : show i
  | otherwise        = show i


