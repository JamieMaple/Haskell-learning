# 总结篇

> `Functor` 是可以映射的东西，`Applicative` 是 `Functor` 升级版，`Monad` 是 `Applicative` 升级版

``` haskell
-- Functor
class Functor  (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

-- fmap == <$>

-- Applicative 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m a) -> m a
    (>>) :: m a -> m b -> m b
    -- x >> y = x >> \_ -> y
    fail :: String -> m a
    fail msg = error msg
```

### 常见的 `Functor`

1. `Maybe Functor`

2. 类似 `Tree` 容器的 `Functor`

3. `Either Functor`

4. `List Functor`

5. `IO Functor`

6. `Function Functor`

7. 一些常见的 `Monoid Functor`

### 常见的 `Applicative`

1. `Maybe Applicative`

2. `List Applicative`

3. `IO Applicative`

4. `ZipList Applicative`

5. `Function Applicative`

### 常见的 `Monad`

1. `Maybe Monad`

2. `List Monad`

3. `MonadPlus`

4. `Writer Monad`

5. `Diff List` (差分列表)

6. `Reader Monad`

7. `State Monad`

> 比较特殊的是可以使用 `do notation`

