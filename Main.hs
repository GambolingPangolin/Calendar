{-
	The goal of this small program is to manage a text calendar. 
	FEATURES
		- Copy upcoming appointments to current.text (we can overwrite current 
		  each time)
		- Move old appointments from calendar.text and current.text to 
		  past_YYYY.text except for repeating apointments which are updated as 
		  the pass.
-}

-- Pseudocode for the parser
-- header >> loop parseInterval indent=0 | parsePoint indent=0
-- parsePoint n = (n tab) >> dateTime >> body >> ?note >> repeat >> newLine >> loop parsePoint (n+1) 

import Data.List (partition)

import Text.Parsec            as P
import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec.Char       as PC

import Structure (Entry(..), Date(..), Time(..), Repeat(..))

inputFile = "/home/ian/Dropbox/calendar.text"
ouputFile = "/home/ian/Code/Haskell/Calendar/output.text"
pastFile  = "/home/ian/Code/Haskell/Calendar/past.text"

main :: IO ()
main = 
  do
    res <- parseFromFile 
             (try (string "CALENDAR" >> newline) >> many (entry 0))
             inputFile
    case res of 
      Left err ->
        print err
      Right cal ->
        let
          -- Take out the repeating items
          -- (reps, rem) = partition isRepeating cal
          -- (past, cur) = partition (isPast today) rem 
          -- newCal      = update reps cur
        in
        --writeFile outputFile $ show newCal >>
        --writeFile pastFile $ show pastCal
        print cal
  
-- PARSERS
date :: Parser Date
date =
  let
    r = read :: String -> Int
  in
  Date
    <$> (r <$> many digit)
    <*> (r <$> (char '-' >> many digit))
    <*> (r <$> (char '-' >> many digit))

time :: Parser Time
time =
  let
    r = read :: String -> Int
  in
  Time
    <$> (r <$> many digit)
    <*> (r <$> (char ':' >> many digit))

repetition :: Parser Repeat
repetition =
  let
    daily   = char 'd' >> (return Daily)
    weekly  = char 'w' >> (return Weekly)
    monthly = char 'm' >> (return Monthly)
    yearly  = char 'y' >> (return Yearly)
  in
  char '^' >> (daily <|> weekly <|> monthly) 

parseNote :: Parser String
parseNote =
  between (char '(') (char ')') (many $ noneOf "\n)")

entry :: Int -> Parser Entry
entry n =
  let
    tryMaybe = optionMaybe . try
    subEntries = newline >> many (entry (n+1))
  in
  count n (char '\t')
  >> Entry
    <$> date
    <*> tryMaybe (spaces >> time)
    <*> tryMaybe (spaces >> date)
    <*> tryMaybe (spaces >> time)
    <*> (spaces >> many (noneOf "\n^("))
    <*> tryMaybe (spaces >> parseNote)
    <*> tryMaybe (spaces >> repetition)
    <*> (try subEntries <|> return [])
