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



