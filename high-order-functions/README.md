# 高阶函数

> 可以接收函数作为参数或者将函数作为返回值的函数称为高阶函数

1. `curry`

没错，本质上 haskell 所有函数都只有一个参数

    1. `Int -> Int -> Int` == `Int -> (Int -> Int)`

``` haskell
let mulTwoWithNine = multTree 9
multTwoWithNine 2 3

let compareWithHundrad = compare 100
```

可以应用部分参数

    2. `section` 截断

    3. 打印函数

柯里化的函数 ghci 不知道怎么在屏幕上显示，也非 `Show` 类型类的实例，无法得知他的字符串表示

形如 `+` 这样的函数，他会先计算然后调用 `show` 转化成字符串

2. 函数作为参数

``` haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)
```

    1. `zipWith`

类似 `zip` 但是可以传入一个函数处理两个 list

    2. `flip`

交换两个参数的位置
``` haskell
flip' :: (a - > b -> c) -> b -> a -> c
flip' f y x = f x y
```


3. 函数式程序员工具箱

    1. `map`

`map :: (a -> b) -> [a] -> [b]`

有些时候和 `list comperhesion` 完全等价

    2. `fliter`

有些时候也可以使用 `list comperhesion`  相互替代

例如前面 `quickSort` 所使用的 `list comperhesion` 完全可以使用 `filter` 取代

4. 映射带有多个参数的函数也是可以的

``` haskell
let listOfFuncs = map (*) [0..]
(listOfFuncs !! 4) 5
```

## lambda

> lambda 就是一次性的匿名函数

写法：`(\函数参数 -> 函数体)` 习惯将他括起来

不要过度使用 lambda

很多 lambda 函数可以由部分应用函数取代

也可以使用模式匹配，但无法设置多个模式

## 折叠

1. `foldl` 左折叠，参数：二元函数，一个初始值以及待折叠的列表

2. `foldr` 右折叠，参数同上，但是二元函数的参数顺序与上面相反

当函数出现类似 `foo a = bar b a` 时，可以改为 `foo = bar b`，因为有柯里化

3. `foldl1` 和 `foldr1` 类似上面的两个函数，不过参数不用指定初始值，其初始值已经为第一个或（最后一个）元素

4. 另一个角度看

    对于一个 list 而言（[3,4,5,6]）

    右折叠：`f 3 (f 4 (f 5 (f 6 z)))`

    左折叠：`g (g (g (g z 3) 4) 5) 6`

5. 无限列表的折叠

6. 扫描

    1. `scanl` 和 `scanr` 与上面的 `fold` 函数类似，但是记录返回但是每次累加值的结果

    2. 不同的是 `scanl` 最终将结果是列表元素的最后一个元素，而 `scanr` 则是结果列表的第一个元素

## `$` 函数应用


1. `$` 这个美元符号叫做 `函数应用符` 

``` haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

普通函数应用 `space` 拥有最高级的优先级，而 `$` 可以让函数低优先级


    1. `$` 操作符是右结合的操作符

所以可以将 `sum (map sqrt [1..30])` 改写成 `sum $ map sqrt [1..30]`

    2. `$` 还可以将函数应用转化为函数，所以允许我映射一个函数应用到一组函数组成的列表上

``` haskell
map ($ 3) [(4+),(10*),(^2),sqrt]
```

## 函数组合（function composition）

``` haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f $ g x
```

具有右结合性

1. 应用场景：随时可以用来生成新函数，这种方式与在函数里面再去声明 `lambda` 是等价的

2. 带有多个参数函数的组合

3. 多个括号结尾可以使用函数组合的方式减少括号

4. `Point-Free` 风格（point-less）

``` haskell
-- 之前科利华的 Point-Free 风格代码
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- function compositing
fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50
```

