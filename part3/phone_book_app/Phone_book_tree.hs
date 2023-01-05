module Phone_book_tree
  ( addEntry,
    findEntries,
    Phonebook (Empty),
  )
where

import Phone_type2

type Name = String

data Phonebook = Empty | Node String [Phone] Phonebook Phonebook deriving (Show, Eq)

entryExists :: Phone -> [Phone] -> Bool
entryExists searchPhone phoneBook = not (null [entry | entry <- phoneBook, phoneNo entry == phoneNo searchPhone])

addEntry :: Name -> String -> String -> String -> Phonebook -> Phonebook
addEntry newName phoneType countryCode phoneNo Empty = Node newName [readPhone phoneType countryCode phoneNo] Empty Empty
addEntry newName phoneType countryCode phoneNo (Node currNodeName phoneList leftNode rightNode)
  | newName == currNodeName = if entryExists newPhone phoneList then Node currNodeName phoneList leftNode rightNode else Node currNodeName (newPhone : phoneList) leftNode rightNode
  | newName == currNodeName =  Node currNodeName (if entryExists newPhone phoneList then phoneList else newPhone : phoneList) leftNode rightNode
  | newName < currNodeName = Node currNodeName phoneList (addEntry newName phoneType countryCode phoneNo leftNode) rightNode
  | newName > currNodeName = Node currNodeName phoneList leftNode (addEntry newName phoneType countryCode phoneNo rightNode)
  where
    newPhone = readPhone phoneType countryCode phoneNo

findEntries :: Name -> Phonebook -> [Phone]
findEntries searchName Empty = []
findEntries searchName (Node currNodeName phoneList leftNode rightNode)
  | searchName == currNodeName = phoneList
  | searchName < currNodeName = findEntries searchName leftNode
  | searchName > currNodeName = findEntries searchName rightNode
