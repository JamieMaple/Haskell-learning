import qualified Data.Map as Map

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int } deriving (Eq, Show, Read)

data Car = Car { company :: String
               , model   :: String
               , year    :: Int
               } deriving (Show)

mikeD  = Person { firstName = "Michael", lastName = "Diamond", age  = 43}
adRock = Person { firstName = "Adam", lastName    = "Horovitz", age = 41 }
mca    = Person { firstName = "Adam", lastName    = "Yauch", age    = 44 }

mysteryDude  = "Person { firstName = \"Michael\"" ++
               ", lastName = \"Diamond\"" ++
               ", age  = 43 }"

data Vector a = Vector a a a deriving (Show)

-- two vector add
vplus  :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector l m n) = Vector (i + l) (m + j) (k + n)

-- two vector dot mult
dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector i j k) (Vector l m n) = i * l + j * m + k * n

-- two vector mult
vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) m = Vector (i * m) (j * m) (k * m) 

-- day
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- type alias
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name,pnum) `elem` pbook

type AssocList k v = [(k,v)]

-- locker
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState,Code)

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNum map = case Map.lookup lockerNum map of
    Nothing -> Left $ "Locker" ++ show lockerNum ++ " doesn't exist!"
    Just (state,code) -> if state /= Taken
                         then Right code
                         else Left $ "Locker" ++ show lockerNum ++ " is Already taken!"


