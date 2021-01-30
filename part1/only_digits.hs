--  A function onlyDigits :: String -> Bool that, given a string, checks whether the string contains only digits or not. 
-- Empty string should return false.

onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits x = null [y | y <- x, y `notElem` ['0'..'9']]