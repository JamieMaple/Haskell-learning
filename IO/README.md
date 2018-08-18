# Input and Output

> Haskell  是一门纯函数语言，不存在副作用，但是不代表 Haskell 没法处理副作用。相反，Haskell 有专门的系统将纯粹与非纯粹的分离了出来

## Hello World

`putStrLn` -> IO () ：取一个字符串作为参数，返回一个 `I/O action`  而它返回的是 `()` 空元组

## 组合 IO 操作

整个程序仅触发一次 IO 操作，利用 `do` 将多个操作整合在一起，但是 `do` 代码块不能为最后的操作绑定名字

`getLine` -> IO String ：返回一个字符串

IO 操作的时候只能通过 `<-` 才能读取 IO 操作的内容，不能使用 `=` 否则就只是一个别名，并且只有在 IO 操作上下文中才能读取 IO 操作的内容

只要 IO 操作最后被组合刀 main 中就会执行
 
绑定普通值可以使用 `let`

`main` 里面（IO 操作）的 `return` 与其他语言不大一样，它能够基于一个纯的值构造一个 IO 操作，并且也不会中断 IO do 代码块的执行

`return` 与 `<-` 做相反的操作，在 `IO` 操作中通常都是用来创建一个什么都不做的 IO 操作

几个实用的 IO 函数：

1. `putStr`

2. `putChar`

3. `print`

取一个 `Show` 类型实例作为参数使之字符串化然后输出终端，相当于 `putStrLn . show`

GHCi 实际上就调用的 `print`

4. `when`

类似控制流

``` haskell
-- 取一个布尔值和一个 IO 操作
when (input == "helloworld") $ do
    putStrLn input
```

5. `sequence`

取一组 IO 操作，返回一个 IO 操作，将列表 IO 依次执行

``` haskell
-- map print [1,2,3]
-- 得到的是一组 IO 操作组成的列表即 [print 1, print 2, print 3]
-- 使用 sequence 才能够转换成一个单独的列表

sequence $ map print [1,2,3]
-- return [(), (), (), ()]

```

6. `mapM`

`mapM` 有点类似于 `sequence . map` 的语法糖？

`mapM_` 不会保留最终结果，不会返回任何列表

7. `forever`

取一个 IO 操作作为参数，返回一个永远重复执行该 IO 操作的 IO 操作

8. `forM`

与 `mapM` 类似，但是参数列表与其相反







