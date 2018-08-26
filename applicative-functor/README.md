# Applicative 函子

## IO 函子

``` haskell
fmap :: (a -> b) -> IO a -> IO b

fmap (reverse . map toUpper) getLine
```

## 函数函子

``` haskell
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
fmap :: (a -> b) -> f a -> f b
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
fmap :: (a -> b) -> (r -> a) -> (r -> b)
-- fmap = . -- compositing
```

`lifting` (提升)：fmap 就是一个提升操作，转换了函数参数

> Lifting is a concept which allows you to transform a function into a corresponding function within another (usually more general) setting.

1. 接受函数和函子值，在函子值上映射这个函数

2. 接受函数，把它提升为操作函子值的函数

-- 类似 Functor f => f a -> f [a] 意味着这个函数能处理任何函子，取决于函子

## 函子定律

1. 所有函子必须表现为可以被映射的

2. 在函子上调用 `fmap` 只做映射不做其他事


### 定律一

在函子值上做 `id` 函数的映射，返回的函子值应该与原先的值一样

``` haskell
fmap id = id -- 形式上
```

### 定律二

如果将两个映射函数组合起来映射到一个函子上，结果上分别按顺序映射结果相同

``` haskell
fmap (f . g) = fmap f . fmap g
```

### 违反函子

在使用或者自定义函子时应当注意不要违反上述两个定律，否者不是函子，代码潜伏着风险

## applicative 函子

函子映射接受有两个（多个？）参数的函数

结合柯里化和部分应用理解

``` haskell
fmap (++) (Just "Hey")

-- Control.Applicative
class (Functor f) => Applicative f where
    pure :: a -> fa
    (<*>) :: f (a -> b) -> f a -> f b
```

1. `pure`: 接受任意类型并返回了一个包裹该值的 applicative 值的一元函数

2. `<*>`: 接受一个是含有函数的函子以及另一个函子，从第一个函数函子中取出函数映射到第二个函子的值中

``` haskell
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> other = fmap f other
```

如果我们在 `applicative`  上下文中处理 `Maybe` 那我们就可以直接用 `pure`

有了 `applicative` 函子之后，我们就可以用一个函数操作若干函子了

我们也可以串起来 `<*>` 无缝操作多个 `applicative` 函子

``` haskell
-- Just elem <*> Just 4 <*> [1,2,3]
pure elem <*> Just 3 <*> [1,2,3]
-- `<*>` 是左结合的
-- Just False

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

3. 风格演进：

``` haskell
-- Maybe 为例
Just f <*> Just a <*> Just b <*> Just c
pure f <*> Just a <*> Just b <*> Just c
f <$> Just a <*> Just b <*> Just c
```

4. 列表：

``` haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x |  f <- fs, xs <- xs]
```

同样是左结合，但是列表会组合出所有的情况

``` haskell
pure f <*> xs -- [f] <*> xs -- fmap f xs
pure f -- [f]
```

对于列表 `pure f <*> xs` 相当于 `fmap f xs`

5. IO:

``` haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

两个 `applicative` 值之间使用 `<*>` 结果还是一个 `applicative` 值

6. 函数：

``` haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

-- pure
(pure 3) "blah"
pure 3 "blah"
-- result: 3

-- <*> 
-- 接受两个参数，可以看得到
```

7. `ZipList`

``` haskell
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> fx) fs xs)

getZipList :: ZipList a -> [a]
-- 转化 ZipList 成 array

-- (,) 等同于 (\x y -> (x,y))，可以同理生成多元组
```

除 `zipWith` 以外，还提供了 `zipWith{n}(3<=n<=7)` 一簇函数，将 n 个列表捆绑到一起，但是 使用 `ZipList` 的 `applicative` 风格可以任意列表数量

8. applicative 定律

``` haskell
pure f <*> x = fmap f x = f <$> x

pure id <*> u = u
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x  = pure (f x)
u <*> pure y = pure ($ y) <*> u
```

### Applicative 实用函数
``` haskell
-- Control.Applicative liftA2
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c

-- sequenceA
sequenceA :: (Applicative f) => [f a] => f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- sequenceA === sequence!!
```



