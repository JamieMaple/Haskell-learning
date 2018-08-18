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
-- | 代表或
```

显式：`Just 3 :: Int`

`Nothing` 类型是 `Maybe a` 所以它是多台的类型


> 然而在 haskell 中有一项严格的约定，那就是永远不要在 data 中添加类约束

## 派生类型

> 类型类更像是接口，而非传统 OOP 类的蓝图

1. 实现 `Eq`：会先检查值构造器是否一致，然后检查其中每一对字段数据是否相等

2. 实现 `Read` 和 `Show`：`Show` 类型可以让派生类能够在屏幕上打印出来，`Read` 类型能从字符串中**通过类型注释** 转化成想要的类型

3. 实现 `Ord`：对于拥有多个值构造器的类型，定义在前面的更小

    所以 `True` > `False`，`Maybe` 类也可以比较大小，`Nothing` 最小

    但是函数无法比较大小，并不是 `Ord` 类的实例

4. `Bounded` `Enum` 等

``` haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- 所有值构造器都是空元（nullary）
```

`Enum` 类型：`succ` 获得后继，`prev` 获取前驱

## 类型别名

1. `[Char]` 和 `String` 等价就是基于类型别名

``` haskell 
type String = [Char]
```

应当注意的是 haskell 中类型别名并不会创造新类型，`data` 创建新类型

2. 参数化类型别名

``` haskell 
type AssocList k v = [(k,v)]

findVal :: (Eq k) => k -> AssocList k v -> Maybe v

type IntMap = AssocList Int -- 柯里化部分应用
```

注：类型构造器与值构造器是不一样的

所以以下不正确

``` haskell
Float 1
AssocList [(1,2)]
```

而应当这样

``` haskell
1 :: Float
[(1,2)] :: AssocList Int Float
```

类型别名只能在 Haskell 的类型部分在使用。类型部分包括了 `data`、`type` 以及 `类型声明(::)`

2. `Either`

``` haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- 一般 Left 的类型表示错误的类型，而 Right 代表正确的
```

## 递归数据结构

1. List（形如链表）

``` haskell
data List a = Empty | Cons a (List a) deriving (Eq, Read, Show, Ord)

-- equal to
data List a = Empty | Cons { ListHead :: a, ListTail :: List a } deriving (Eq, Read, Show, Ord)
```

其中 `Empty` 相当于 `[]`，`Cons` 相当于 `:`

``` haskell
-- 实现标准库的 List
infixr 5 :
data List a = Empty | a : (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ++
(++) :: List a -> List a -> List a
Empty  ++ ys = ys
(x:xs) ++ ys = (:) x (xs ++ ys)
```

模式匹配本质上就是对值构造器的匹配，所以也可以对一些基本值进行值构造器的匹配

2. 树形结构

``` haskell
-- binary search tree
data Tree a = EmptyTree | Node (Tree a) (Tree a) deriving (Show)
```

注：haskell 的数据结构不能像 c 等某些语言那样使用直接使用指针，haskell 只有值

树的一些列操作不像其他语言那样，而是返回一个新树（purity），这样并不低效，因为底层有优化


## 类型类

1. Eq 类型类：

``` haskell
-- 定义一个新类型类 Eq，其中 a 为类型变量
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-- 函数体的实现是可选的，必须要的是类型声明
-- 函数体的是以交叉递归实现的
-- 该种风格被称为类型类的最小完备定义
-- 函数体并非函数的实际类型
```

2. 手工实现类型类

``` haskell
instance <Class> <Type> where
-- implementation
```

`Show` 类型的最小完备定义要求实现 `show` 函数

3. 子类化

``` haskell
-- 类型约束，更是子类化
class (Eq a) => Num a where
```

4. 作为类型实例的带参数类型

``` haskell
instance (Eq m) => Eq (Maybe m) where
    ....
```

查看某个类型类的定义以及实例：`:info {type}`，对类型和类型构造器都有效

## Yes-No 类型类

Haskell 严格检查 Bool，但是我们可以实现有一种类似 JS 的布尔隐式类型转化功能

``` haskell
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id -- `id` 是标准库内将参数原样返回的函数
-- ....
```

## Functor 类型类

> 可以用来映射的事物

``` haskell
-- 其中 f 是取一个参数的类型构造器
class Functor f where
    fmap :: (a -> b) -> f a  -> f b
-- 很类似 list 的 map 函数
```

凡是拥有容器性质的类都可以视作 `Functor`

类似树的数据结构（或者说容器），也是一种 `Functor`

但是如果对二叉搜索树做映射没法保证依然是二叉搜索树

``` haskell
-- instance Functor (Either a) where
    fmap :: (b -> c) -> Either a b -> Either a c

-- instance Functor (Map.Map k) where
    fmap :: (v -> v') -> Map.Map k v -> Map.Map k v'
```

## kind 和无名类型

1. 类型构造器与函数类似，同样可以柯里化与部分应用

2. 函数同时也是值，有自己的类型，类型也有类型也就是 `Kind`

3. `:k {type}` 可以在 GHCi 中查看类型的 `Kind`，其中 `*` 代表了具体类型

4. `:t` 检查值的类型，`:k` 检查类型的 `Kind`，可以类似看各自身上的标签



