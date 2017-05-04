{-|	Structures for the calendar parsing app

-}

module Structure (
  Entry(..)
  , Date(..)
  , Time(..)
  , Repeat(..)
  , repeating
  ) where

import Control.Monad (liftM)
import Data.Function ((&))
import Data.List (intercalate, concat)
import Data.Maybe (isJust, maybe)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

-- Preferred syntax for reverse function application
(|>) = (&)

{-| A calendar entry has the form
 -  STARTDATE [STARTTIME] [ENDDATE] [ENDTIME] BODY [(NOTE)] [REPEAT]

-}
data Entry = Entry
  { startDate :: Date
  , startTime :: Maybe Time
  , endDate :: Maybe Date
  , endTime :: Maybe Time
  , body :: String
  , note :: Maybe String
  , rep :: Maybe Repeat
  , subEntries :: [Entry]
  } deriving Eq

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
    [ [show (startDate e)]
    , mss (startTime e)
    , mss (endDate e)
    , mss (endTime e)
    , [body e]
    , maybe [] (\ s -> ['(' : (s ++ ")")]) $ note e
    , mss (rep e)
    ]
    |> concat 
    |> intercalate " "
    |> flip (++) "\n"
    |> flip (++) se 

{-| We store dates as a record containing the month, day, and year.
 
 -}
data Date = Date
  { month :: Int
  , day :: Int
  , year :: Int 
  } deriving Eq

instance Ord Date where
  d1 <= d2 = 
    year d1 < year d2
    || (year d1 == year d2 && month d1 < month d2)
    || (year d1 == year d2 && month d1 == month d2 && day d1 <= day d2)

instance Show Date where
  show d =
    intercalate "-" (map show [month d, day d, year d])

{-| Get the current date, formatted as a Date

 -}
today :: IO Date
today =
  let
    fromTuple (y,m,d) = Date m d (fromInteger y)
  in
  liftM (fromTuple . toGregorian . utctDay) getCurrentTime

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
    show (hour t) ++ ":" ++ (show $ minute t) 

{-| An event can repeat daily, weekly or monthly.
 -  TODO: Add a constructor for more complex repetition.

-}
data Repeat = 
  Daily 
  | Weekly 
  | Monthly 
  | Yearly
  deriving Eq

instance Show Repeat where
  show r =
    case r of 
      Daily   -> "^d"
      Weekly  -> "^w"
      Monthly -> "^m"
      Yearly  -> "^y"

{-| Check if an entry has a set repeat.

-}
repeating :: Entry -> Bool
repeating = isJust . rep
