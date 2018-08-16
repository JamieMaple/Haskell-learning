# Learning haskell (2)

## 高阶函数

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

