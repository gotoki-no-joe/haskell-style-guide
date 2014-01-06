Haskell スタイルガイド
======================

https://github.com/tibbe/haskell-style-guide の翻訳である。
命名と形式に関する広い範囲に言及した。
本ガイドで言及されていない事項に関しては、他のモジュールのコードに合わせること。

形式
----

### 行の長さ

1行は*80文字*以下とする。

### 字下げ

タブは使わない。字下げには空白文字を用いる。
ブロックは*4文字*下げる。
予約語 `where` は本体から2文字下げ、その定義内容はさらに2文字下げる。以下に例を示す。

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

### 空行

トップレベル定義は空行1行で区切る。
型シグネチャと関数定義の間には空行を入れない。
型クラスのインスタンス宣言と関数の間は、関数本体が大きい場合には1行開ける。（各自で判断してよい）

### 空白

二項演算子の両側には空白を1文字開ける。
算術演算子の周りの空白は自己判断でよいが、二項演算子の両側の空白には一貫性を持たせる。
λの後には空白を入れない。

### データ型宣言

データ型宣言の構成子は位置揃えする。例：

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

型名が長い場合は以下の形式でもよい。

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

レコードも同様にする：

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### リスト定義

リストの要素を桁揃えする。例：

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

初めに改行しない方法もある。好みで選択してよい。

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### プラグマ

プラグマは適用する関数の直後に置く。例：

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

データ型宣言の場合は、適用する型の直前に置く。例：

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### ぶら下がりλ (Hanging Lambdas)

「ぶら下がり」λは、字下げしてもしなくてもよい。各自で判断してよい。例を示す。

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### 輸出一覧

輸出一覧は次のように整形する。

```haskell
module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
```

### if-then-else 節

全般的に、可能ならガードとパターンマッチがif節より望ましい。
短い場合分けは1行に入れる（行の長さに収まるならば）。

モナドを用いない（すなわち`do`を使わない）コードで、
ガードとパターンマッチが使えない場合は、
通常の式にするようにif-then-else節を字下げしてよい。

```haskell
foo = if ...
      then ...
      else ...
```

それ以外の場合は4文字下げ規則に従い、予約語`then`と`else`は揃える。例：

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

doブロックの入れ子にも同じ規則を適用する：

```haskell
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
```

### case 式

case式の選択肢は次の2つの形式のいずれかに整形する。

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

または

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

可読性の向上のために`->`を揃える。

輸入
----

輸入は次の順に分類する：

1. 標準ライブラリの輸入
2. 関連するサードパーティの輸入
3. アプリケーションまたはライブラリ固有の輸入

輸入の各分類は空行で区切る。
分類ごとにモジュール名によりアルファベット順に整列させる。

標準ライブラリとサードパーティのライブラリについては必ず、
明示的な輸入一覧を付けるか`qualified`にする。
これは、ライブラリ側の変化に対して自分のコードを頑健にする。
ただしPreludeは例外とする。

注釈
----

### 句読法

正しい文を書く。(英語の場合は)大文字で始め、句読点を正しく使う。

### トップレベル定義

全てのトップレベル関数、特に輸出する関数には注釈を付け、型シグネチャを付ける。
コメントはHaddock記法に従う。
輸出するデータ型には注釈をつける。
関数の例を示す。

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

関数に対する説明は、関数の定義を見なくても関数が利用できるように充分な情報を与える。

レコードの例：

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

長い注釈が必要なフィールドは次のように整形する：

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text
      
      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

### 行末注釈

行末注釈とコードは空白を2文字開ける。
データ型定義の注釈は桁揃えする。
例を示す：

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### リンク

インラインリンクはふんだんに入れる。
API名にリンクを付けることを奨励する。
Haddockコメント中の全てのAPI名にリンクを付ける必要はない。
よって、次の場合にそうする：

* （主観的に）利用者が実際にクリックして情報を得たいらしい箇所
* コメント中で各API名が現れる最初の場所のみ（重複を気にする必要はない）

命名
----

関数名もデータ型名も、キャメルケースを使う。
（例：`functionName`, `DataType`）

可読性のために、省略語も全て大文字にはしない。
例えば、`HTTPServer`ではなく`HttpServer`とする。
2文字の省略語は例外とする。例：`IO`

### モジュール

モジュール名は単数形でつける。
例：
`Data.Map`とし、`Data.Maps`としない。
`Data.ByteString.Internal`とし、`Data.ByteString.Internals`としない。

遅延評価の扱い
--------------

他に理由がない場合、データ型は正格に、関数は遅延評価にする。

### データ型

構成子のフィールドは、遅延にする明確な理由がない限り正格にするべきである。
これは遅延の過多に起因する多くのよくある過ちを避け、
プログラマが評価順序について悩む時間を減らす。

```haskell
-- よい
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- 悪い
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

さらに、単純なフィールドをunpackすると性能は向上しメモリ使用量は減少することが多い。

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

`UNPACK`プラグマの代わりに、ファイルの先頭で次のようにしてもよい。

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

このフラグをファイルそのものに入れることは、
他の方法、例えばCabalファイルに入れる方法より好ましい。
誰かが他の方法でそのファイルをコンパイルした場合でも最適化が適用されるからである。
（つまり、最適化指示がソースコードそのものに付いている。）

`-funbox-strict-fields`は小さなフィールド（例：`Double`や`Int`）だけでなく、
あらゆる正格なフィールドに適用されることに注意せよ。
GHC 7.4以降であれば、`-funbox-strict-fields`により適用されるunpackを
`NOUNPACK`により選択的に取り消すことができる。

### 関数

関数の引数は正格にする明確な必要性がない限り遅延評価にする。

明確に正格引数が必要なよくある場合とは、蓄積引数を伴う再帰である。

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

その他
------

### ポイントフリースタイル ###

ポイントフリースタイルは過剰にしない。
例えば、以下は読みにくい。

```haskell
-- 悪い
f = (g .) . h
```

### コンパイラによる警告 ###

コードは`-Wall -Werror`を付けてコンパイルできるべきである。
警告は出てはいけない。
