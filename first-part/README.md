# Haskell-learning
坑准备。。。。

### terminal

`ghci`

对于想要类型声明，可以使用 `:set +m`

对于多条语句可以使用 `:{` 以及 `:}`

`~/.haskeline` 添加 `set editing-mode vi` 可开启 `vi` 模式

[more ghci](https://downloads.haskell.org/~ghc/7.2.2/docs/html/users_guide/interactive-evaluation.html)

### 一切都是函数

`*` 类似这样的操作符被称为`中缀函数`.

函数调用拥有最高优先级

函数转成中缀形式：**4 `min` 5**

函数名字可以加入单引号

无参数的函数被称为`定义`或`名字`(definition or name)


### list

声明变量：`let a = []`

拼接数组：`[1, 2, 3] ++ [4, 5, 6]`

前插元素：`5:[1,2,3]`

取索引：`[1,2,3] !! 2`

可作比较，从第一个元素开始比较大小

`head` 取得 list 头部

`tail` 取得 list 尾部，也就是去除掉头部之后的部分

`last` 取得最后一个元素

`init` 返回一个 list 除去最后一个元素

其他函数：`reverse` `take` `maximum` `sum` `elem`

range

`[1..20]`

反向：`[20, 19..1]`

可以生成无限数组

本质上是通过两个数计算到步长

### list comprehension

`[ x*2 | x <- [1..10], x*2 >= 12 ]`

`length' xs = sum [ 1 | _ <- xs ]`


### tuple

`(1,2)`

`[(1,2,3), (2,3,5i)]`


`fst` 返回序对的首项

`snd` 返回序对的尾项

函数式编程思路：先取一个初始的集合并将其变形，执行过滤条件

### types

1. `:t {variables}`

2. 函数是第一等公民，函数有自己的签名，多个参数的函数看得出来柯里化过，以 `->` 为分隔，最后一个为返回值的类型

     例如：`circumference :: Float -> Float`

3. `Int` 有界（bounded），64位 CPU 一般范围 [-2^63, 2^63-1]

4. `Integer` 无界整数，但是效率不如 `Int`

5. `Float` 单精度浮点数

6. `Double` 双精度浮点数
7. `Bool` -> `True` `False`

8. `Char` 表示的是一个 Unicode 字符

9. 元祖，空元祖也是类型，只有一个值 `()`

10. 类型变量，形如 `head` 等函数其参数是类型变量

11. 类型类是定义行为的接口，`=>` 类型约束必须相同类型

    1. `Eq` 类型类： 判断相等性，`==` 和 `/=`(不等)

    2. `Ord` 类型类： 比较大小，`>` `<` `>=` `<=`

    3. `Show` 类型类：可以表示为字符串的类型， 除函数意外其他都是 Show 类型的示例，最常用的是 `show` 可以转实力类型到字符串

    4. `Read` 类型类：与 `Show` 相反的类型类，可以取字符串做参数然后转为某个实例的类型 

    5. `Enum` 类型类：可以美剧的类型主要有 `()` `Bool` `Char` `Ordering` `Int` `Integer` `Float` `Double`

    6. `Bounded`  类型类：`maxBound` 和 `minBound`，如果元组中项的类型都属于 Bounded 类型类的实例，那么该元组也属于 `Bounded` 实例

    7. `Num`  类型类：`Int` `Double` 等，只有已经属于 `Show` 和 `Eq` 实例的类型才能成为 `Num` 类型的实例

    8. `Floating` 类型类：`Float` 和 `Double`

    9. `Integer` 类型类：`Int` 和 `Integer`

### 函数

#### 模式匹配

``` haskell
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```

调用 `lucky` 时会自动将传入的参数从上往下的检查各个模式

万能模式:  `n|x|y|(lowerCaseLetter)`

2. 元组的模式匹配

比如可以计算空间向量

3. 列表与列表推导式的模式匹配

比如 [1,2,3] 可以看做是 1:2:3:[] 的语法糖

4. `As` 模式

#### 哨位

1. `|`

``` haskell
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal, Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
```

应该会优于一堆 `if else` 命令式的语句

#### `where` ?!

``` haskell
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're a whale, congulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
```

可以适当保留一部分中间变量方便使用

1. `where` 中定义的变量只对本函数可见，其他模式不可见，因此不用担心污染到全局变量

2. `where` 中也可以进行模式匹配

3. `where` 中甚至可以定义函数

``` haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
```

4. `let`

`let` 表达式，创建局部变量，也可以使用模式匹配

1. 格式：`let <bindings> in <expressions>` 在 `let` 中绑定的变量仅对 `in` 部分是可见的

2.  常见的用法：

    1.  局部作用域中定义函数：`let square x = x * x in square 10`

    2.  分号可以在一行中分开绑定多个变量

    3.  从元组中取值：`let (a,b,c) = (1,2,3) in a * b * c`

    4. `let` 和 `where` 有区别

    5. 列表推导式中使用，可以用于绑定关键字

3. GHCi 中使用 `let`

    1. 省略 `in` 相当于很大范围的作用域？

    2. 没有省略 `in` 会有返回值，不接收 GHCi 就会打印

#### `case` 表达式

函数定义的模式匹配本质上就是 `case` 表达式的语法糖

1. 语法结构：

``` haskell
case expression of pattern -> result
                   pattern -> result
                   pattern -> result
```

2. 适用场景：anywhere

### 递归

> 如果你还不明白，就读这句话

命令式语言要求告知如何计算，函数式要求声明什么样的问题

重要的不是求解步骤二是定义问题与解的描述

模式匹配加上 `Recursion` 非常常见

1. `replicate`

2. `take`

3. `repeat`

4. `reverse`

5. `repeat`

    注：haskell 函数是惰性的，能够获取无限列表，只要能保证在某位置截断它

6. `zip`

7. `elem`
    注：书上只是 `Eq` 类型类的实例，而我们 `:t elem` 可以看到还有一个 `Foldable t`

#### 排序

5 行快排，一生无悔入 haskell

``` haskell
quickSort :: (Eq a) => [a] -> [a]
quickSort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quickSort quickSortOrEqual ++ [x] ++ quickSort larger
```

#### 总结

对于递归的思路，首先找基准条件，应对特殊的输入简单非递归函数

然后分解成一个或多个子问题并递归地调用自身



