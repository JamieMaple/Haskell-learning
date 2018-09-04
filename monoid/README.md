# Monoid

## `newtype`

``` haskell
-- data
data ZipList a = ZipList { getZipList :: [a] }

-- newtype
newtype ZipList a = ZipList { getZipList ：： [a] } deriving (Eq, Show)

-- 包裹与解包
ZipList :: [a] -> ZipList
getZipList :: ZipList -> [a]
```

1. `newtype` 只是将一个现有的类型包裹成一个新类型，与 `data` 不同，它不会有包裹和解包的开销

2. `newtype` 只能有一个值构造器，该值构造器也只能有一个字段

3. 可以像 `data` 一样使用 `deriving` 关键字

4. `newtype` 由于只是把一个已有的类型变成一个新类型，所以他的模式匹配的惰性比 `data` 深

    `undefined` 表示错误的计算

    `newtype` 进行模式匹配是转换值的类型，而 `data` 是从容器里面取出东西

5. `type` `data` 和 `newtype` 的区别

    `type` 只是别名，具体没区别（可能只是为了更好的可读性）

    `data` 可以创建数据类型，实现任何袋鼠数据类型

    `newtype` 可以想象成只有一个值构造器与一个字段的 `data` 声明，声明记录语法时，我们也就得到了新旧类型转换的函数

## monoid 举例

一个 `monoid` 类型类由一个满足结合律的二元函数与一个单位元组成，满足属性：

1. 函数接受两个参数

2. 参数与返回值的类型相同

3. 存在一个这样的值：与二元函数一起使用时返回值与另一个参数相同

4. 满足结合律

``` haskell
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

`monoid` 定律

``` haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

### 列表 

列表就是 `monoid`

``` haskell
instance Monoid [a] where
    mempty = []
    mappend = (++)

-- mempty -> []
-- mconcat -> concat
```

### `Product` 和 `Sum`

``` haskell
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Num => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

newtype Sum a = Product { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Num => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)
```

### `Any` 和 `All`

``` haskell
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid All where
    mempty = All true
    All x `mappend` All y = All (x && y)
```

### `Ordering monoid`

`Ordering` 有三个值：`LT`、`EQ` 以及 `GT`

``` haskell
instance Monoid Ordering where
    mempty = Eq
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
```

### `Maybe monoid`

``` haskell
instance Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

newtype First a = First { getFirst :: Maybe a }
instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
```

## `Monoid` 折叠

很多数据结构适合折叠，类型类：`Foldable`

``` haskell
foldl (*) 1 [1,2,3]  -- 7
foldr (*) 2 (Just 3) -- 6

foldr :: Foldable a => (a -> b -> b) -> b -> t a -> b

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
```



