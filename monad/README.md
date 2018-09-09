# Monad

### `Monad` 类型类

``` haskell
class Monad m where
    return :: a -> m a

    (>>=) :: m -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
```

### `do` notation

``` haskell
-- nested
foo :: Maybe String
foo Just 3 >>= (\x -> 
    Just "!" >>= (\y ->
    Just Show x ++ y))

-- do notaion
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just show x ++ y
-- 看起来有点像命令式的语法
```

`do` 只是 `monad` 值串起来的一种不同语法

每一个不带 `let` 的行都是一个 `monad` ，可以使用 `<-` 获取到值

最后一个 `monad` 值是作为整个 `do` 表达式的值

`do` 中如果某一行不带 `<-` 可以视作在想忽略的值后面加上了 `>>`

#### 模式匹配和计算失败

一般来说，模式匹配失败会进入下一个模式，如果所有的都失败就会抛出错误，程序奔溃

`let` 表达式里面的模式匹配失败会立刻发生错误，因其并没有错误处理机制

`do` 来说，默认情况下会崩溃，但是有些时候可以产生表示失败的计算

``` haskell
wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x
-- Nothing
```

### 列表 Monad

``` haskell
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = [] -- 非确定性计算，表示失败的 Monad 值
```

*MonadPlus* 表示具有 `monoid` 行为的 `monad`

``` haskell
class Monad => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
```

列表推导式是列表 `Monad` 的语法糖

``` haskell
-- module: Control.Monad
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero
```

多种方式示例：

``` haskell
-- list comprehesion
[x | x <- [1..50], '7' `elem` x]

-- guard
[1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

-- guard with do
sevensOnly :: [Int]
sevensOnly = do
    x <- [0..50]
    guard ('7' `elem` show x)
    return x
```

## `Monad` 定律

### 左单元

> The first monad law state that if we take a value,
> put it in a default context with `return` and then feed it to  a function by using `>>=`,
> it's the same as just taking the value and applying the function to it.

``` haskell
return x >>= f -- same as `f x`
```

*PS: `IO` 使用 `return` 会产生一个没有副作用的 `IO` 操作*

**

### 右单元

> The second law states that if we have a monadic value and we use `>>=` to feed it to `return`,
> the result is our original moadic value

``` haskell
m >>= return -- same as `m`
```

既不会引入任何错误信息(Maybe)，更不会引入额外的非确定性(List)

### 结合律

> The final law says that when we have a chain of monadic function application
> with `>>=`, it shouldn't matter how they are nested.

``` haskell
(m >>= f) >>= g
-- equals to
m >>= (\x -> f x >>= g)

-- Control.Monad
-- Monad composition
f <=< (g <=< h)
-- same with
(f <=< g) <=> h
```

## 其他 `Monad`

### `Writer`

用来表示附有日志的值，所有日志都会被合并一起并附在结果值上

``` haskell
applyLog :: (a, String) -> (a -> (b, String))
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- update with monoid
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)
```

#### `Writer` 类型类

``` haskell
-- Control.Monad.Writer
-- 使用 `newtype` 有别于普通的类型
newtype Writer w a = Writer { runWriter :: (a,w) }

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y,v')) = f x in Writer (y, v `mappend` v')

writer :: MonadWriter w m => (a, w) -> m a -- 接受一个包含有值和 `monoid` 的二元组，返回一个 `monad`
tell :: w -> m () -- 接受一个 `monoid` 返回一个 `()` 的 `monad`
```

`Writer` 也可以配合 `do` 实现

**切记低效的列表构造**，通常在写或者改造函数的时候尽量采取右结合的方式

#### 差分列表 

> 注意：`ghci` 8.4v 有所变化，要得到 `Monoid` 约束同时需要得到 `Semigroup` 的约束
> 猜测 `mappend` 效果同 `<>` 是一样的

``` haskell
-- instance list monoid
-- Data.Semigroup
instance Semigroup [a] where
    (<>) = (++)
-- Data.Monoid
instance Monoid [a] where
    mempty = []
-- [参考](https://wiki.haskell.org/Monoid)
```

### `Reader`

``` haskell
-- Monad function
instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w
```

由于都从一个共同源读取参数，函数 `monad` 也被称为 `Reader monad`

``` haskell
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

addStuff :: Int -> Int
addStuff x = let
    a = (*2) x
    b = (+10)
    in a+b
```

### 带状态的优雅表示

> Haskell is a pure function and because of that, our programs are made of
> functions that cannot change any global state or variables, they can only
> do some computations and return their results.(immutable)

特殊的状态 `monad` (State Monad)

带状态的计算其实就是一个函数：取一个状态，返回一个值和新的状态

``` haskell
s -> (a, s)
-- s 是状态的类型，a 是带状态的结果类型

-- Control.Monad.State
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State $ \s -> (a,s )
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState


-- exmaples
import Control.Monad.State

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop
```

`state` 函数与 `State` 值构造器功能相同

`runState` 用于解开 `State` 包裹并获取内部被包裹的函数

``` haskell
-- Control.Monad.Stack
get = state $ \s -> (s, s)
put newState = state $ \s -> ((), newState)
```

随机性和状态的使用
``` haskell
-- random useage
import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random
```

### Error

`ghc` 8.4v 有所变化，`Control.Monad.Error` 是被反对的， 改用 `Control.Exception`

``` haskell
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
```

有些时候需要显式添加 `Either String Int` 类型

### 实用的 `Monad` 函数

``` haskell
-- 如果一个类型的 Functor 实例和 Monad 实例
-- 都满足相应的定律，以下两个函数功能相同
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))
-- do notation
leftM f m = do
    x <- m
    return (f x)

-- 类似 `fmap`
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

`liftM` 的实现不涉及到 `Functor` 所以只要 `monad` 提供东西就可以实现 `fmap`，所以它不弱于函子


``` haskell
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)

-- 类似 `<*>`
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
```

很多时候先写出 `Monad` 实例然后再把 `pure` 和 `<*>` 分别定义为 `return` 以及 `ap` 就能实现 `Applicative` (目前 `ghc` 8.4v `Monad` 继承自 `Applicative`)

也可以吧 `fmap` 定义为 `liftM` 从而实现 `Functor`

``` haskell
-- liftA2 在两个 `applicative` 之间应用一个方便的函数
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- join 用于展平 `Monad`
join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m -- m 本身就已经是个 `Monad` 值了
-- 对于 List， `join` 本身就是 `concat`

-- filterM 
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

-- foldM
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```

### 组合 `Monad` 函数

``` haskell
let f = (+1) . (*2)
f 4

-- monad examples
let g = (\x -> return (x+1)) <=< (\x -> return (x*2))
Just 4 >>= g
```

可以通过使用 `>=>` 以及 `<=<` 来对组合 `Monad` 函数

### 创建 `Monad`






