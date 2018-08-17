# 模块（module）

> Haskell 中的模块指的是包括了函数、类型与类型类的定义的文件

PS: 如果分离的代码足够独立，相互之间没有过多依赖 -- 松耦合（loosely coupled）

之前我们能在 GHCi 环境下调用的模块、类型以及类型类都是 `Prelude` 模块的和一部分，默认情况下，`Prelude` 是会自动被导入的

## 导入模块

`import <optional>qualified {moduleName} <optional>hidding <optional>(...funcs) <optional>(as {alias})` 导入模块必须放在任何函数定义之前

`GHCi` 中也可以通过 `:m + {...moduleName}` 导入模块

`Hoogle` 可以查阅相关模块、函数等的文档


1. `foldl` 是惰性的，不到万不得已不会主动计算，所以折叠的时候会保留很大的延迟计算栈

2. `Data.List` 中 `foldl'` 是严格版费惰性的折叠函数

3. `digitToInt` 能够转化 '0' 到 '9' 以及 'A-F' (或者小写) 到数字

4. `Maybe` 类型与 `[a]` 类型相似，它可能是 `Nothing` 还可能是 `Just {something}`

