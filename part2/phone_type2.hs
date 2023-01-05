data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Ord, Eq, Read)

data CountryCode = CountryCode Integer deriving (Eq, Ord)

instance Show CountryCode where
  show (CountryCode num) = '+' : show num

fromCountryCode :: CountryCode -> Integer
fromCountryCode (CountryCode code) = code

toCountryCode :: Integer -> CountryCode
toCountryCode code
  | code < 0 = error "Negative country code"
  | otherwise = CountryCode code

instance Num CountryCode where
  fromInteger = toCountryCode
  x + y = let r = fromCountryCode x + fromCountryCode y in toCountryCode r
  x - y =
    let r = fromCountryCode x - fromCountryCode y
     in if r < 0 then error "Negative country code" else toCountryCode r
  x * y = let r = fromCountryCode x * fromCountryCode y in toCountryCode r

data PhoneNo = PhoneNo Integer deriving (Eq, Ord)

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
  | num < 0 = error "Negative phone number"
  | otherwise = PhoneNo num

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo num) = num

instance Num PhoneNo where
  fromInteger = toPhoneNo
  x + y = let r = fromPhoneNo x + fromPhoneNo y in toPhoneNo r
  x - y =
    let r = fromPhoneNo x - fromPhoneNo y
     in if r < 0 then error "Negative phone number" else toPhoneNo r
  x * y = let r = fromPhoneNo x * fromPhoneNo y in toPhoneNo r

instance Show PhoneNo where
  show (PhoneNo num) = show num

data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

instance Show Phone where
  show (Phone phoneType countryCode phoneNo) = show countryCode ++ " " ++ show phoneNo ++ " (" ++ show phoneType ++ ")"

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phone code num
  | code < 0 = error "Negative country code"
  | num < 0 = error "Negative phone number"
  | otherwise = Phone {phoneType = phone, countryCode = code, phoneNo = num}
