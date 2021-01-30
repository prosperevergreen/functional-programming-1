-- Write a function validate :: String -> Bool that, given a string validates the string as a Finnish IBAN code.

-- For details, see https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN.

-- You will also need the following information:

-- Length of a Finnish IBAN code is 18.
-- Finnish IBAN begins with the country code FI.
-- All the characters after the country code are digits.


validate :: String -> Bool
validate x
    | length x /= 18 = False
validate (x:y:z) 
    |   x /= 'F' || y /= 'I' = False
    |   not (null [a | a <- z, a `notElem` ['0'..'9']]) = False
validate (a:b:c:d:e)
    |   (read (e ++ "1518" ++ [c,d])::Integer) `mod` 97 /= 1 = False
validate _ = True