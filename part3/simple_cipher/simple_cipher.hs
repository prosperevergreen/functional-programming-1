import Data.Char

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
   in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

readMaybeInt :: String -> Maybe Int
readMaybeInt st = case reads st :: [(Int, String)] of
  [(x, "")] -> Just x
  _ -> Nothing

listCoder :: Maybe Int -> [String] -> (Int -> String -> String) -> [String]
listCoder (Just shift) wordList coderFunc = map (\x -> coderFunc shift x) wordList


readCoder :: String -> IO ()
readCoder wordsStr
  | length wordsList > 2 && coderType `elem` ["encode","decode"] && shiftNum /= Nothing = putStrLn $ unwords $ listCoder shiftNum codeWordsList (if coderType == "encode" then encode else decode)
  | otherwise = putStrLn "I cannot do that"
  where
    wordsList = words wordsStr
    coderType = head wordsList
    shiftNum = readMaybeInt $ wordsList !! 1
    codeWordsList = tail $ tail wordsList

main = do
  line <- getLine
  case line of
    "quit" -> do
      putStrLn "bye"
      return ()
    _ -> do
      readCoder line
      main