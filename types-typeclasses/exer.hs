data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     , height    :: Int
                     , phoneNum  :: String
                     , flavor    :: String } deriving (Show)

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

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

