{-| The goal of this small program is to manage a text calendar. 
	  FEATURES
		- Copy upcoming appointments to current.text (we can overwrite current 
		  each time)
		- Move old appointments from calendar.text and current.text to 
		  past_YYYY.text except for repeating apointments which are updated as 
		  the pass.
    - There should be a file 'config' in the same directory as Main which has a 
      single line containing the path to the calendar files

    TODO: Handle cases where input files are empty
-}

import Data.Char (isAlphaNum, isPunctuation)
import Data.List (partition, intercalate, sort)
import Data.Time.Calendar

import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec.Char

import Debug.Trace (traceShow)

import Structure

calFile = "calendar.text"
pastFile  = "past.text"

main :: IO ()
main = 
  do
    rootPath <- filter (\ x -> isPunctuation x || (isAlphaNum x)) 
                <$> readFile "config"

    res <- parseFromFile 
            (try (string "CALENDAR" >> newline) >> many (entry 0))
            (rootPath ++ calFile)

    t <- today

    case res of 
      Left err ->
        print err

      Right cal ->
        let

          -- Take out the repeating items
          (reps, rem) = partition isRepeating cal
          -- Take out the past items
          (past, cur) = partition (isPast t) rem 

          -- Update repeats and combine
          newCal      = sort $ map (update t) reps ++ cur 

          outputNew   = 
            "CALENDAR\n"
            ++ (concat (map show newCal))

          outputPast  =
            concat $ map show $ sort $ filter (isPast t) reps ++ past

        in
        writeFile (rootPath ++ calFile) outputNew
        >> appendFile (rootPath ++ pastFile) outputPast
  
-- PARSERS

{-| Parse a date.

-}
date :: Parser Day
date =
  let
    rInteger = read :: String -> Integer
    rInt     = read :: String -> Int
  in
  fromGregorian
    <$> (rInteger <$> many digit)
    <*> (rInt <$> (char '-' >> many digit))
    <*> (rInt <$> (char '-' >> many digit))

{-| Parse a time.

-}
time :: Parser Time
time =
  let
    r = read :: String -> Int
  in
  Time
    <$> (r <$> many digit)
    <*> (r <$> (char ':' >> many digit))

{-| Parse the repetition field.

-}
repetition :: Parser Repeat
repetition =
  let
    daily   = char 'd' >> (return Daily)
    weekly  = char 'w' >> (return Weekly)
    yearly  = char 'y' >> (return Yearly)
  in
  char '^' >> (daily <|> weekly <|> yearly) 

{-| Parse a note.

-}
parseNote :: Parser String
parseNote =
  between (char '(') (char ')') (many $ noneOf "\n)")

{-| Parse an entry.

-}
entry :: Int -> Parser Entry
entry n =
  let

    onlySpaces = many $ char ' '
    tryMaybe   = optionMaybe . try
    bodyParser = intercalate " " <$> endBy (many1 $ noneOf " \n\r^(") onlySpaces
    subEntries = newline >> many (try $ entry (n+1))
    
  in
  count n (char '\t')
  >> Entry
    <$> date
    <*> tryMaybe (onlySpaces >> time)
    <*> tryMaybe (onlySpaces >> date)
    <*> tryMaybe (onlySpaces >> time)
    <*> (onlySpaces >> bodyParser)
    <*> tryMaybe (onlySpaces >> parseNote)
    <*> tryMaybe (onlySpaces >> repetition)
    <*> subEntries

-- HELPERS

{-| Check if the Entry should be considered past with respect to the Date.

-}
isPast :: Day -> Entry -> Bool
isPast t e =
  case endDate e of
    Just d -> 
      d < t
    Nothing ->
      startDate e < t

{-| Update an entry if it is repeating.

-}
update :: Day -> Entry -> Entry
update t e =
  case rep e of
    Nothing -> e
    Just r ->
      let
        d = diffDays t (startDate e)
      in
      case (isPast t e, r) of
        (False, _)  -> e
        (_,Daily)   ->
          e { startDate = t 
            , endDate   = fmap (addDays d) (endDate e)
            }
        (_,Weekly)  ->
          let
            n = case d `rem` 7 of
                  0 -> d 
                  r -> d + 7 - r
          in
          e { startDate = addDays n (startDate e)
            , endDate   = fmap (addDays n) (endDate e)
            }
        (_,Yearly)  ->
          e { startDate = addGregorianYearsClip 1 (startDate e)
            , endDate   = fmap (addGregorianYearsClip 1) (endDate e)
            }
        
-- TRACE
tr x = traceShow x x
