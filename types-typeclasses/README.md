# 构造我们自己的类型与类型类

## 定义新的数据类型

``` haskell
-- 左端为类型名称，右端为值构造器（value constructor）
data Bool = False | True
    deriving (Show)
```

1. 类型名和值构造器的首字母必须大写

2. 值构造器本质上就是一个返回某数据类型的函数

3. 值构造器也可以进行模式匹配

4. 导出数据类型：

``` haskell
module moduleName
( Type(Sub1, Sub2)
, Type(..)
, Type
) where
```

括号中可以导出值构造器，也可以导出所有，也可以不导出值构造器，内部实现进

比如 `Data.Map` 的类型就没有导出任何值构造器

## 记录语法

用于构造字段比较多又不容易分辨的地方，同时不关心字段的顺序和位置

不仅方便代码的阅读，也更优雅，可以自动生成函数，根据字段取值

``` haskell
data Person = Person { firstName :: String
    , lastName                   :: String
    , age                        :: Int
    , height                     :: Int
    , phoneNumber                :: String
    , flavor                     :: String } deriving (Show)
```

## 类型构造器

> 类型构造器取类型作为参数并产生新的类型（🤔可能类似于泛型）

``` haskell
data Maybe a = Nothing | Just a
-- 这里的 a 就是一个类型参数
```

显式：`Just 3 :: Int`

`Nothing` 类型是 `Maybe a` 所以它是多台的类型


> 然而在 haskell 中有一项严格的约定，那就是永远不要在 data 中添加类约束


