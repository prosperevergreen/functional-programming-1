import qualified Data.List as L
import Date ( correctDate, makeDate, Date )

data EventInfo = EventInfo {name :: String, place :: String, date :: Date} deriving (Eq)

instance Show EventInfo where
  show event = "Event " ++ name event ++ " happens at " ++ place event ++ " on " ++ show (date event)

instance Ord EventInfo where
  event1 `compare` event2 = name event1 `compare` name event2

main = loop $ return []

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
      if length inputList /= 5 || unwords (take 3 inputList) /= "Tell me about"
        then badInput
        else
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
      if length inputList < 4 || unwords (take 2 inputList) /= "What happens"
        then badInput
        else
          if "at" `elem` inputList
            then --   Command to fine event on date
              if null evtDate
                then putStrLn "Bad date"
                else
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
        evtDate = readDate $ tail $ init $ last inputList
        evtsPlace = L.sort $ [ev | ev <- events, place ev == evtPlace]
        evtsDate = L.sort $ [ev | ev <- events, date ev == head evtDate]
    _ -> do
      badInput
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
printEventDate (evt : list) = do
  putStrLn ("Event " ++ name evt ++ " happens at " ++ show (date evt))
  printEventDate list

printEventPlace :: [EventInfo] -> IO ()
printEventPlace [] = return ()
printEventPlace (evt : list) = do
  putStrLn ("Event " ++ name evt ++ " happens on " ++ place evt)
  printEventPlace list

badInput :: IO ()
badInput = do
  putStrLn "I do not understand that. I understand the following:"
  putStrLn "*Event <name> happens at <place> on <date>"
  putStrLn "*Tell me about <eventname>"
  putStrLn "*What happens on <date>"
  putStrLn "*What happens at <place>"
  putStrLn "*Quit"