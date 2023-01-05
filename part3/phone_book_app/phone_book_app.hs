import Phone_book_tree

addPhoneBookEngine :: String -> Phonebook -> Phonebook
addPhoneBookEngine phoneActionStr phonebook
  | length phoneActionList == 5 = addEntry name phoneType countryCode phoneNo phonebook
  | otherwise = Empty
  where
    phoneActionList = words phoneActionStr
    action = head phoneActionList
    name = phoneActionList !! 1
    phoneType = phoneActionList !! 2
    countryCode = phoneActionList !! 3
    phoneNo = phoneActionList !! 4

printPhoneBookEngine :: String -> Phonebook -> IO ()
printPhoneBookEngine phoneActionStr phonebook
  | length phoneActionList == 2 = print (findEntries name phonebook)
  | otherwise = putStrLn "Cannot do that"
  where
    phoneActionList = words phoneActionStr
    name = last phoneActionList

phoneBookMachine :: Phonebook -> IO ()
phoneBookMachine phoneBook = do
  line <- getLine
  let action = head $ words line
   in case action of
        "quit" -> do
          putStrLn "bye"
          return ()
        "add" -> do
          putStrLn "Done"
          phoneBookMachine newPhoneBook
          where
            newPhoneBook = addPhoneBookEngine line phoneBook
        "find" -> do
          printPhoneBookEngine line phoneBook
          phoneBookMachine phoneBook
        _ -> do
          putStrLn "Cannot do that"
          phoneBookMachine phoneBook

main = do
  putStrLn "Welcome to phone book application"
  phoneBookMachine Empty