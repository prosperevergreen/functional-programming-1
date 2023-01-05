readMaybeInt :: String -> Maybe Integer
readMaybeInt st = case reads st :: [(Integer, String)] of
  [(x, "")] -> Just x
  _ -> Nothing

readCalc :: String -> IO ()
readCalc calcStr
  | length calcArr == 3 = calc num1 num2 sign
  | otherwise = putStrLn "I cannot calculate that"
  where
    calcArr = words calcStr
    num1 = readMaybeInt $ head calcArr
    sign = calcArr !! 1
    num2 = readMaybeInt $ last calcArr

calc :: Maybe Integer -> Maybe Integer -> [Char] -> IO ()
calc Nothing _ _ = putStrLn "I cannot calculate that"
calc _ Nothing _ = putStrLn "I cannot calculate that"
calc (Just num1) (Just num2) sign
  | sign == "+" = print (num1 + num2)
  | sign == "-" = print (num1 - num2)
  | sign == "*" = print (num1 * num2)
  | otherwise = putStrLn "I cannot calculate that"

main = do
  line <- getLine
  case line of
    "quit" -> do 
        putStrLn "bye"
        return ()
    _ -> do 
        readCalc line
        main
