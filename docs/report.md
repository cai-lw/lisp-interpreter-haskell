# 《函数式语言程序设计》课程作业实验报告

本项目为《函数式语言程序设计》课程的期末作业，内容为用Haskell实现简单的类Lisp语言解释器。

## 功能简介

本项目实现了一个纯函数式的、动态强类型的Lisp子集的解释器（由于解释器的名称为ki，我们不妨称这门语言为K语言），支持完全的lambda calculus，并支持布尔、浮点数、字符、字符串等类型的字面量定义和基本运算，在类型不符时给出运行时错误提示。



按作业要求的得分点来看，本项目实现了以下功能：

| 实现的功能          | 难度星数 |
| -------------- | ---- |
| 独立主程序          | 1    |
| REPL           | 1    |
| 解释器            | 1    |
| 文法解析           | 2    |
| Pretty Printer | 1    |
| 错误处理           | 1    |
| 代码测试           | 2    |
| 代码风格           | 3    |
| 总计             | 12   |

按作业要求的得分点来看，本项目实现了以下语言特性：

| 实现的语言特性 | 难度星数 |
| ------- | ---- |
| 逻辑表达式   | 1    |
| 算术表达式   | 1    |
| 字符串与列表  | 2    |
| 文法作用域   | 3    |
| 高阶函数    | 4    |
| 总计      | 11   |



我们认为，本项目的核心亮点在于采用了类似finally tagless style [[1]](#ref1) [[2]](#ref2)的EDSL（嵌入式领域特定语言，即利用宿主语言特性实现的，可以与宿主语言混合书写的语言）实现lambda calculus。在定义了基本的操作符之后 ，可以直接把语法树翻译成形式相同的EDSL，而后者可根据lambda calculus的规则自动演算，既免去了手动维护栈、变量名映射等状态的麻烦，又实现了解释器和语义定义的分离。

## 实现简介

### 语法解析(`Klang.Parse`)

语法解析器的实现主要借助attoparsec——一个轻量化的monadic parser库。目标是将源代码文本解析成用如下ADT表达的S-expression语法树：

```haskell
data SExp = Lit Data            -- 字面量
          | Var String          -- 变量
          | Func String [SExp]  -- 函数调用（lambda函数的应用也视作一个函数）
```

monadic parser结合Haskell的do notation使得parser的构建非常简便。例如，以下是parse一个字符串字面量的parser：

```haskell
do
    _ <- char '"'
    s <- many' $ (char '\\' >> char '"') <|> (char '\\' >> char '\\') <|> satisfy (/='"')
    _ <- char '"'
    return (Lit $ foldr (curry KPair) KNil (KChar <$> s))
```

### 类型定义(`Klang.Type`)

用如下ADT表达K语言中用到的所有数据类型：

```haskell
data Data =
    KBool Bool
    | KDouble Double
    | KChar Char
    | KPair (Data, Data)
    | KNil
    | KLam (Res -> Res)

type Res = Either String Data -- Left表示求值时发生错误，错误信息的类型为String
```

可以看到，K语言中的每种类型都对应一种Haskell原生类型。因此，我们定义以下typeclass，以便在Haskell原生类型和K语言类型之间转换：

```haskell
class KType a where
    pack :: a -> Data                  -- 将Haskell类型转换成Data
    unpack :: Data -> Either String a  -- 将Data转换成Haskell类型，转换失败时返回错误信息

instance (KType a, KType b) => KType (a, b) where  -- 以比较复杂的Pair类型为例
    pack (x, y) = KPair (pack x, pack y)
    unpack (KPair (kx, ky)) = do
        x <- unpack kx
        y <- unpack ky
        return (x, y)
    -- typeString :: Data -> String 返回该类型的名称
    unpack d = Left $ "Expect pair, found " ++ typeString d
```

### EDSL设计(`Klang.Lambda`)

Finally Tagless[[1]](#ref1)原本是有类型（直接使用宿主语言类型）lambda calculus的EDSL，由于类型系统的限制，其中的变量只能用de Brujin index（可粗略理解为“使用变量处与定义变量处之间套了多少层lambda”）表示。在本项目中，我们将Finally Tagless改造为无类型（所有数据都属于`Res`类型）lambda calculus的EDSL，书写形式几乎不变，且在减弱了类型系统的限制之后可以直接用变量名索引变量。篇幅所限，这里不再介绍原本的Finally Tagless，以下将介绍本项目实现的EDSL。

定义如下数据类型：

```haskell
-- 每个K h a是lambda calculus中的一个项(term)，含义为：提供类型为h的环境，执行run求值，结果是类型a
newtype K h a = K { run :: h -> a }

-- 这里借鉴了Codewars网友（链接见参考文献[2]）的解答
-- 只有在参与运算的term均不改变环境的前提下，(K h)才是Applicative，相当于一个“装着返回值的箱子”
-- 在无类型条件下，这个定义是不严谨的，只是为了方便实现运算符，不可滥用
instance Applicative (K h) where
    pure = K . const
    f <*> x =  K $ \h -> run f h (run x h)

instance Functor (K h) where
    fmap f = (<*>) (pure f)

-- Lambda calculus的环境是一个栈，在无类型的情况下可以用List表示
type Env = [(String, Res)]

-- K语言中的项，总是接受类型为Env的环境，返回类型为Res
type KTerm = K Env Res
```

然后就可以定义lambda calculus的基本运算：

```haskell
slot :: String -> KTerm                -- 给定变量名，定义一个变量
slot s = K $ \h -> case lookup s h of  -- 求值时，从栈顶到栈底查找与自己名字相同的记录
    Just x -> x
    Nothing -> Left ("Cannot find variable \"" ++ s ++"\"")

lambda :: String -> KTerm -> KTerm  -- 给定变量名和函数体，定义一个lambda函数
-- 被apply时，将变量名和传入的参数压入栈中，再对函数体求值
lambda s t = K $ \h -> (Right . KLam) $ \x -> run t ((s, x) : h)

apply :: KTerm -> KTerm -> KTerm  -- 应用一个lambda函数
apply = liftA2 $ \kf ->           -- 这里liftA2将Res -> Res -> Res提升为KTerm -> KTerm -> KTerm
    case kf >>= unpack  of
        Right (f :: (Res -> Res)) -> f  -- 做运行时类型检查
        Left e -> const $ Left e        -- 如果被应用的类型不是lambda则返回错误信息
```

使用方法如下：

```haskell
-- lit :: Data -> KTerm 用于定义字面量，addOp :: KTerm -> KTerm -> KTerm 是加法运算符
let plus2 = lambda "x" (addOp (slot "x") (lit $ KDouble 2.0))
let test = apply plus2 (lit $ KDouble 3.0) -- 相当于(λx.x+2)3
run [] test -- 初始环境为空栈，故传入[]
-- 输出：Right (KDouble 5.0)
```

### 运算符定义(`Klang.Ops`)

定义一些辅助函数后，可以直接把Haskell原生函数提升成作用于KTerm的运算符

```haskell
liftR2 :: (KType a, KType b, KType c) => (a -> b -> c) -> Res -> Res -> Res
liftR2 f ekx eky = do
    x <- ekx >>= unpack    -- 这里已经隐含了类型检查，如果unpack不出对应的Haskell类型会直接返回错误
    y <- eky >>= unpack
    return $ pack (f x y)

-- 将类型正确的二元Haskell原生函数提升成作用于KTerm的二元函数。KFunc2即KTerm -> KTerm -> KTerm
liftK2 :: (KType a, KType b, KType c) => (a -> b -> c) -> KFunc2
liftK2 = liftA2 . liftR2

addOp :: KFunc2
addOp = liftK2 ((+) :: Double -> Double -> Double) -- 就是这么简单。同理可以一行定义其他运算符
```

定义运算符后，需要在`Klang.Config`中注册其字符串名称，供解释器使用。

```haskell
kFunc1 :: [(String, KFunc1)] -- 全体一元函数的列表，同理有kFunc2
kFunc1 = [
    ("not", notOp),
    ("car", carOp),
    ("cdr", cdrOp)]
```

### “编译”器(`Klang.Compile`)

这里的“编译”指的是将语法树翻译成上面提到的EDSL，方法是非常直白的。而且“编译”时不需要知道有哪些内置运算符，随时可以在`Klang.Ops`和`Klang.Config`中修改、添加和注册。

```haskell
-- 以let表达式的编译为例。(let s x y)等价于((lambda s y) x)
compileLet :: [SExp] -> Either String KTerm
compileLet [Var s, sx, sy] = do
    x <- compile sx
    y <- compile sy
    return $ apply (lambda s y) x
compileLet _ = Left "Wrong syntax. Should be (let var expr expr)"

compile :: SExp -> Either String KTerm
compile (Lit x) = Right $ lit x
compile (Var s) = Right $ slot s
compile (Func f args)
    | f == "let" = compileLet args
    | f == "lambda" = compileLambda args
    | f == "apply" = compileF2 ("apply", apply) args
    | isJust found1 = compileF1 (fromJust found1) args
    | isJust found2 = compileF2 (fromJust found2) args
    where found1 = find ((== f) . fst) kFunc1
          found2 = find ((== f) . fst) kFunc2
```

### 主程序

主程序负责处理所有IO工作，包括读取代码文件、输出执行结果、和运行REPL。

一个比较麻烦的问题是在遇到错误时希望提前退出一个do notation。这里我们参考了[[3]](#3) ，采用的是mtl库中的`ExecptT` monad transformer，给IO monad加上抛出异常的功能。下面以main函数的一部分错误处理代码为例说明：

```haskell
exit :: (MonadError () m) => m a
exit = throwError ()

main :: IO ()
main = void $ runExceptT $ do
    (options, args, errors) <- getOpt Permute optionDef <$> lift getArgs -- 读取并解析命令行参数
    unless (null errors) $ do                          -- 如果有解析错误则输出解析错误后退出
        lift $ mapM_ putStrLn errors                   -- lift在这里把IO a提升为ExceptT e IO a
        lift $ putStrLn "type \"ki --help\" to see usage."
        exit
    -- 如果没有错误则继续执行do notation的剩余部分
```

### 测试

用hspec编写测试用例非常方便。作为例子，以下是一部分测试用例：

```haskell
spec :: Spec
spec = do
    describe "simple tests" $ do
        it "arithmetics" $ do
            interpret "(- (* (+ 1 2) 2.5) (/ 5 2))" `shouldBe` Right (KDouble 5.0)
        it "bool and relation" $ do
            interpret "(and (or True False) (not True))" `shouldBe` Right (KBool False)
            interpret "(or (< 123 456) (>= 123 456))" `shouldBe` Right (KBool True)
        it "pair, char and string" $ do
            interpret "(cons 'c' (cdr \"bat\"))" `shouldBe` Right (mkList $ map KChar "cat")
            interpret "(cons (car \"abc\") 1)" `shouldBe` Right (KPair (KChar 'a', KDouble 1.0))
        it "let and lambda" $ do
            interpret "(let x (+ 1 2) (let y (+ 3 4) (* x y)))" `shouldBe` Right (KDouble 21.0)
            interpret "(let id (lambda x x) (id nil))" `shouldBe` Right KNil
```

利用hspec自带的hspec-discover工具，可以自动运行目录下所有.hs文件中的测试代码。运行测试的方法请参阅README。

当然，本项目已经通过了自行编写的所有测试用例的测试。

## 使用方法

请参阅README，此处不再重复。

## 参考文献

<a name="ref1">[1]</a> Kiselyov, O. (2012). Typed tagless final interpreters. In *Generic and Indexed Programming* (pp. 130-174). Springer Berlin Heidelberg.

<a name="ref2">[2]</a> Finally Tagless Interpreter | Codewars. Retrieved from https://www.codewars.com/kata/finally-tagless-interpreter.

<a name="ref3">[3]</a> Gonzalez, G. (2012). Breaking from a loop. In *Haskell for all*. Retrieved from http://www.haskellforall.com/2012/07/breaking-from-loop.html