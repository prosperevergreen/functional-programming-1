import qualified Data.List as L


-- Date.hs functions


-- Note: this is not a complete implementation for dates.
-- This is just a data type example.

-- Define safer date types than (Int,Int,Int)

data Month = MakeMonth Integer deriving (Eq, Show, Ord)

toMonth :: Integer -> Month
toMonth x
  | x < 1 = error "Minimum month number is 1"
  | x > 12 = error "Maximum month number is 12"
  | otherwise = MakeMonth x

fromMonth :: Month -> Integer
fromMonth (MakeMonth i) = i -- Pattern match i out

-- I do not define abs or signum. It seems to go through.
-- I only allow positive values so they are not relevant.

instance Num Month where
  fromInteger = toMonth
  x + y =
    let r = fromMonth x + fromMonth y
     in if r < 1 || r > 12
          then error "Unnatural addition for month"
          else toMonth r
  x - y =
    let r = fromMonth x - fromMonth y
     in if r < 1 || r > 12
          then error "Unnatural subtraction for month"
          else toMonth r
  x * y =
    let r = fromMonth x * fromMonth y
     in if r < 1 || r > 12
          then error "Unnatural multiplication for month"
          else toMonth r

data Day = MakeDay Integer deriving (Eq, Show, Ord)

toDay :: Integer -> Day
toDay x
  | x < 1 = error "Minimum day number is 1"
  | x > 31 = error "Maximum day number is 31"
  | otherwise = MakeDay x

fromDay :: Day -> Integer
fromDay (MakeDay i) = i

instance Num Day where
  fromInteger = toDay
  x + y = toDay $ fromDay x + fromDay y
  x - y = toDay $ fromDay x - fromDay y
  x * y = toDay $ fromDay x * fromDay y

-- this would take care of year 0
data Year = MakeYear Integer deriving (Eq, Show)

toYear :: Integer -> Year
toYear x
  | x == 0 = error "No year 0"
  | otherwise = MakeYear x

fromYear :: Year -> Integer
fromYear (MakeYear i) = i

instance Num Year where
  fromInteger = toYear
  x + y = toYear $ fromYear x + fromYear y
  x - y = toYear $ fromYear x - fromYear y
  x * y = toYear $ fromYear x * fromYear y

{- This does not take care of year 0 ie it allows it

newtype Year = MakeYear Integer deriving (Eq, Show, Ord, Read)

toYear :: Integer -> Year
toYear x
 | x == 0 = error "No year 0"
 | otherwise = MakeYear x

fromYear :: Year -> Integer
fromYear (MakeYear x) = x

-}

data Date = Date {year :: Year, month :: Month, day :: Day} deriving (Eq)

instance Ord Date where
  Date {year = year1, month = month1, day = day1} `compare` (Date year2 month2 day2) = ((10000 * fromYear year1) + (100 * fromMonth month1) + fromDay day1) `compare` ((10000 * fromYear year2) + (100 * fromMonth month2) + fromDay day2)

instance Show Date where
  show date = show currYear ++ "-" ++ if currMonth < 10 then "0" else "" ++ show currMonth ++ "-" ++ if currDay < 10 then "0" else "" ++ show currDay
    where currMonth = fromMonth $ month date
          currDay = fromDay $ day date
          currYear = fromYear $ year date


data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

-- A function to check if a year is a leap year

leapYear :: Year -> Bool
leapYear (MakeYear y)
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

makeMaybeDate :: Integer -> Integer -> Integer -> Maybe Date
makeMaybeDate y m d
  | y == 0 = Nothing
  | elem m [1, 3, 5, 7, 8, 10, 12]
      && elem d [1 .. 31] =
    makeJustDate y m d
  | elem m [4, 6, 9, 11]
      && (elem d [1 .. 30]) =
    makeJustDate y m d
  | m == 2 && elem d [1 .. 28] = makeJustDate y m d
  | leapYear (toYear y) && m == 2 && d == 29 = makeJustDate y m d
  | otherwise = Nothing
  where
    makeJustDate y m d = Just $ makeDate y m d

-- 3: Write a function to check if a given date (y,m,d)
-- is correct

correctDate :: Integer -> Integer -> Integer -> Bool
correctDate 0 _ _ = False
correctDate y m d
  | (elem m [1, 3, 5, 7, 8, 10, 12]) && (elem d [1 .. 31]) = True
  | (elem m [4, 6, 9, 11]) && (elem d [1 .. 30]) = True
  | (m == 2) && (elem d [1 .. 28]) = True
  | (leapYear (toYear y)) && (m == 2) && (d == 29) = True
  | otherwise = False

makeDate :: Integer -> Integer -> Integer -> Date
makeDate y m d
  | correctDate y m d = Date {year = toYear y, month = toMonth m, day = toDay d}
  | otherwise = error "not correct combination of integers for year, month and day"

-- 4: Write a function that, given a date,
-- calculates the next date

nextDate :: Date -> Date
nextDate date
  | correctDate y m (d + 1) = Date {year = year date, month = month date, day = toDay (d + 1)}
  | correctDate y (m + 1) 1 = Date {year = year date, month = toMonth (m + 1), day = toDay 1}
  | y == (-1) = Date {year = toYear 1, month = toMonth 1, day = toDay 1}
  | otherwise = Date {year = toYear (y + 1), month = toMonth 1, day = toDay 1}
  where
    y = fromYear $ year date
    m = fromMonth $ month date
    d = fromDay $ day date


data EventInfo = EventInfo {name :: String, place :: String, date :: Date} deriving (Eq)

instance Show EventInfo where
  show event = "Event " ++ name event ++ " happens at " ++ place event ++ " on " ++ show (date event)

instance Ord EventInfo where
  event1 `compare` event2 = name event1 `compare` name event2
  
  
  
  
-- Calender.hs functions

-- main = loop $ return []

loop :: IO [EventInfo] -> IO ()
loop ioEvents =
  do
    input <- getLine
    if input == "Quit"
      then putStrLn "bye"
      else doCommand input ioEvents

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents --Now you can use events as [EventInfo]
  case command of
    --   Command for adding new event
    "Event" -> do
      if null evDate
        then do
          putStrLn "Bad date"
          loop $ return events
        else do
          putStrLn "ok"
          if null event
            then loop $ return $ EventInfo {name = evName, place = evPlace, date = head evDate} : events
            else loop $ return $ map (\x -> if name x == evName then EventInfo {name = evName, place = evPlace, date = head evDate} else x) events
      where
        evName = tail $ init $ unwords [inputList !! 1, inputList !! 2]
        evPlace = tail $ init $ unwords [inputList !! 5, inputList !! 6]
        evDate = readDate $ tail $ init $ last inputList
        event = [ev | ev <- events, name ev == evName] -- Test if event exists already

    --   Command for findinf event by name
    "Tell" -> do
      if null event
        then putStrLn "I do not know of such event"
        else print evt
      loop $ return events
      where
        evName = tail $ init $ unwords [inputList !! 3, inputList !! 4]
        event = [ev | ev <- events, name ev == evName]
        evt = head event

    --   Command for on date or at place
    "What" -> do
      if "on" `elem` inputList
        then --   Command to fine event on date
          if null evtsDate
            then putStrLn "Nothing that I know of"
            else printEventDate evtsDate
        else --   Command to find event at place
          if null evtsPlace
          then putStrLn "Nothing that I know of"
          else printEventPlace evtsPlace
      loop $ return events
      where
        evtPlace = tail $ init $ unwords [inputList !! 3, inputList !! 4]
        evtDate = head $ readDate $ tail $ init $ last inputList
        evtsPlace = L.sort $ [ev | ev <- events, place ev == evtPlace]
        evtsDate = L.sort $ [ev | ev <- events, date ev == evtDate]
    _ -> do
      putStrLn "I do not understand that. I understand the following:"
      putStrLn "*Event <name> happens at <place> on <date>"
      putStrLn "*Tell me about <eventname>"
      putStrLn "*What happens on <date>"
      putStrLn "*What happens at <place>"
      putStrLn "*Quit"
      loop $ return events
  where
    inputList = words input
    command = head inputList



readDate :: String -> [Date]
readDate dataStr
  | length dataStr == 10 && correctDate year month day = [makeDate year month day]
  | otherwise = []
  where
    year = read (take 4 dataStr) :: Integer
    month = read (take 2 $ drop 5 dataStr) :: Integer
    day = read (drop 8 dataStr) :: Integer

printEventDate :: [EventInfo] -> IO ()
printEventDate [] = return ()
printEventDate (evt:list) = do
  putStrLn ("Event " ++ name evt ++" happens on " ++ show (date evt))
  printEventDate list

printEventPlace :: [EventInfo] -> IO ()
printEventPlace [] = return ()
printEventPlace (evt:list) = do
  putStrLn ("Event " ++ name evt ++" happens at " ++ place evt)
  printEventDate list
