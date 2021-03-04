newtype PhoneType = MakePhoneType String deriving (Ord, Eq, Read)

instance Show PhoneType where  
    show (MakePhoneType phoneType) = phoneType

readPhoneType :: String -> Maybe PhoneType
readPhoneType phoneType
    | phoneType == "" = Nothing
    | otherwise = Just (MakePhoneType phoneType)

newtype CountryCode = MakeCountryCode Integer deriving (Eq, Ord)

instance Show CountryCode where  
    show (MakeCountryCode num) = '+' : show num

fromCountryCode :: CountryCode -> Integer
fromCountryCode (MakeCountryCode code) = code

toCountryCode :: Integer -> CountryCode
toCountryCode code 
    | code < 0 = error "Negative country code"
    | otherwise = MakeCountryCode code

readCountryCode :: String -> Maybe CountryCode
readCountryCode countryCode
    | countryCode == "" = Nothing
    | otherwise = Just (toCountryCode (read countryCode::Integer))

instance Num CountryCode where  
    fromInteger = toCountryCode
    x + y = let r = fromCountryCode x + fromCountryCode y in toCountryCode r
    x - y = let r = fromCountryCode x - fromCountryCode y in
        if r < 0 then error "Negative country code" else toCountryCode r
    x * y = let r = fromCountryCode x * fromCountryCode y in toCountryCode r


data PhoneNo = PhoneNo Integer deriving (Eq, Ord)

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num 
    | num < 0 = error "Negative phone number"
    | otherwise = PhoneNo num

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo num) = num

readPhoneNo :: String -> PhoneNo
readPhoneNo phoneNo = toPhoneNo (read phoneNo::Integer)

instance Num PhoneNo where  
    fromInteger = toPhoneNo
    x + y = let r = fromPhoneNo x + fromPhoneNo y in toPhoneNo r
    x - y = let r = fromPhoneNo x - fromPhoneNo y in
        if r < 0 then error "Negative phone number" else toPhoneNo r
    x * y = let r = fromPhoneNo x * fromPhoneNo y in toPhoneNo r

instance Show PhoneNo where  
    show (PhoneNo num) = show num

data Phone = Phone {phoneType :: Maybe PhoneType, countryCode :: Maybe CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

instance Show Phone where  
    show (Phone (Just phoneType) (Just countryCode) phoneNo) = show countryCode ++ " " ++ show phoneNo ++ " (" ++ show phoneType ++ ")" 
    show (Phone Nothing (Just countryCode) phoneNo) = show countryCode ++ " " ++ show phoneNo
    show (Phone (Just phoneType) Nothing phoneNo) = show phoneNo ++ " (" ++ show phoneType ++ ")" 
    show (Phone Nothing Nothing phoneNo) = show phoneNo

readPhone :: String -> String -> String -> Phone
readPhone phoneType countryCode phoneNum = Phone {phoneType = pType, countryCode = cCode, phoneNo = pNum}
    where pType = readPhoneType phoneType
          cCode = readCountryCode countryCode
          pNum = readPhoneNo phoneNum

makePhone :: PhoneType -> CountryCode  -> PhoneNo -> Phone
makePhone phone code num 
    | code < 0 = error "Negative country code"
    | num < 0 = error "Negative phone number"
    | otherwise = Phone {phoneType = Just phone, countryCode = Just code, phoneNo = num}

