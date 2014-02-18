Haskell スタイルガイド
======================
<!--original
Haskell Style Guide
===================
-->

https://github.com/tibbe/haskell-style-guide の翻訳である。
命名と形式に関する広い範囲に言及した。
本ガイドで言及されていない事項に関しては、他のモジュールのコードに合わせること。
<!-- ここの訳文は原文とは異なる -->
<!--original
This is a short document describing the preferred coding style for
this project.  I've tried to cover the major areas of formatting and
naming.  When something isn't covered by this guide you should stay
consistent with the code in the other modules.
-->

形式
----
<!--original
Formatting
----------
-->

### 行の長さ
<!--original
### Line Length
-->

1行は*80文字*以下とする。
<!--original
Maximum line length is *80 characters*.
-->

### 字下げ
<!--original
### Indentation
-->

タブは使わない。字下げには空白文字を用いる。
ブロックは*4文字*下げる。
予約語 `where` は本体から2文字下げ、その定義内容はさらに2文字下げる。以下に例を示す。
<!--original
Tabs are illegal. Use spaces for indenting.  Indent your code blocks
with *4 spaces*.  Indent the `where` keyword two spaces to set it
apart from the rest of the code and indent the definitions in a
`where` clause 2 spaces. Some examples:
-->

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
<!--original
### Blank Lines
-->

トップレベル定義は空行1行で区切る。
型シグネチャと関数定義の間には空行を入れない。
型クラスのインスタンス宣言と関数の間は、関数本体が大きい場合には1行開ける。（各自で判断してよい）
<!--original
One blank line between top-level definitions.  No blank lines between
type signatures and function definitions.  Add one blank line between
functions in a type class instance declaration if the functions bodies
are large.  Use your judgement.
-->

### 空白
<!--original
### Whitespace
-->

二項演算子の両側には空白を1文字開ける。
算術演算子の周りの空白は自己判断でよいが、二項演算子の両側の空白には一貫性を持たせる。
λの後には空白を入れない。
<!--original
Surround binary operators with a single space on either side.  Use
your better judgement for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator.  Don't insert a space after a lambda.
-->

### データ型宣言
<!--original
### Data Declarations
-->

データ型宣言の構成子は位置揃えする。例：
<!--original
Align the constructors in a data type definition.  Example:
-->

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

型名が長い場合は以下の形式でもよい。
<!--original
For long type names the following formatting is also acceptable:
-->

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

レコードは以下のようにする：
<!--original
Format records as follows:
-->

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### リスト定義
<!--original
### List Declarations
-->

リストの要素を桁揃えする。例：
<!--original
Align the elements in the list.  Example:
-->

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

初めに改行しない方法もある。好みで選択してよい。
<!--original
Optionally, you can skip the first newline.  Use your judgement.
-->

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### プラグマ
<!--original
### Pragmas
-->

プラグマは適用する関数の直後に置く。例：
<!--original
Put pragmas immediately following the function they apply to.
Example:
-->

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

データ型宣言の場合は、適用する型の直前に置く。例：
<!--original
In the case of data type definitions you must put the pragma before
the type it applies to.  Example:
-->

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### ぶら下がりλ
<!--original
### Hanging Lambdas
-->

「ぶら下がり」λは、字下げしてもしなくてもよい。各自で判断してよい。例を示す。
<!--original
You may or may not indent the code following a "hanging" lambda.  Use
your judgement. Some examples:
-->

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
<!--original
### Export Lists
-->

輸出一覧は次のように整形する。
<!--original
Format export lists as follows:
-->

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
<!--original
### If-then-else clauses
-->

全般的に、可能ならガードとパターンマッチがif節より望ましい。
短い場合分けは1行に入れる（行の長さに収まるならば）。
<!--original
Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).
-->

モナドを用いない（すなわち`do`を使わない）コードで、
ガードもパターンマッチも使えない場合は、
通常の式にするようにif-then-else節を字下げしてよい。
<!--original
When writing non-monadic code (i.e. when not using `do`) and guards
and pattern matches can't be used, you can align if-then-else clauses
you like you would normal expressions:
-->

```haskell
foo = if ...
      then ...
      else ...
```

それ以外の場合は4文字下げ規則に従い、予約語`then`と`else`は揃える。例：
<!--original
Otherwise, you should be consistent with the 4-spaces indent rule, and the
`then` and the `else` keyword should be aligned.  Examples:
-->

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
<!--original
The same rule applies to nested do blocks:
-->

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
<!--original
### Case expressions
-->

case式の選択肢は次の2つの形式のいずれかに整形する。
<!--original
The alternatives in a case expression can be indented using either of
the two following styles:
-->

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

または
<!--original
or as
-->

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

可読性の向上のために`->`を揃える。
<!--original
Align the `->` arrows when it helps readability.
-->

輸入
----
<!--original
Imports
-------
-->

輸入は次の順に分類する：
<!--original
Imports should be grouped in the following order:
-->

1. 標準ライブラリの輸入
2. 関連するサードパーティの輸入
3. アプリケーションまたはライブラリ固有の輸入
<!--original
1. standard library imports
2. related third party imports
3. local application/library specific imports
-->

輸入の各分類は空行で区切る。
分類ごとにモジュール名によりアルファベット順に整列させる。
<!--original
Put a blank line between each group of imports.  The imports in each
group should be sorted alphabetically, by module name.
-->

標準ライブラリとサードパーティのライブラリについては必ず、
明示的な輸入一覧を付けるか`qualified`にする。
これは、ライブラリ側の変化に対して自分のコードを頑健にする。
ただしPreludeは例外とする。
<!--original
Always use explicit import lists or `qualified` imports for standard
and third party libraries.  This makes the code more robust against
changes in these libraries.  Exception: The Prelude.
-->

注釈
----
<!--original
Comments
--------
-->

### 句読法
<!--original
### Punctuation
-->

正しい文を書く。(英語の場合は)大文字で始め、句読点を正しく使う。
<!--original
Write proper sentences; start with a capital letter and use proper
punctuation.
-->

### トップレベル定義
<!--original
### Top-Level Definitions
-->

全てのトップレベル関数、特に輸出する関数には注釈を付け、型シグネチャを付ける。
コメントはHaddock記法に従う。
輸出するデータ型には注釈をつける。
関数の例を示す。
<!--original
Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:
-->

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

関数に対する説明は、関数の定義を見なくても関数が利用できるように充分な情報を与える。
<!--original
For functions the documentation should give enough information to
apply the function without looking at the function's definition.
-->

レコードの例：
<!--original
Record example:
-->

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

長い注釈が必要なフィールドは次のように整形する：
<!--original
For fields that require longer comments format them like so:
-->

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
<!--original
### End-of-Line Comments
-->

行末注釈とコードは空白を2文字開ける。
データ型定義の注釈は桁揃えする。
例を示す：
<!--original
Separate end-of-line comments from the code using 2 spaces.  Align
comments for data type definitions.  Some examples:
-->

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
<!--original
### Links
-->

インラインリンクはふんだんに入れる。
API名にリンクを付けることを奨励する。
Haddockコメント中の全てのAPI名にリンクを付ける必要はない。
よって、次の場合にそうする：
<!--original
Use in-line links economically.  You are encouraged to add links for
API names.  It is not necessary to add links for all API names in a
Haddock comment.  We therefore recommend adding a link to an API name
if:
-->

* （主観的に）利用者が実際にクリックして情報を得たいらしい箇所
<!--original
* The user might actually want to click on it for more information (in
  your judgment), and
-->

* コメント中で各API名が現れる最初の場所のみ（重複を気にする必要はない）
<!--original
* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)
-->

命名
----
<!--original
Naming
------
-->

関数名もデータ型名も、キャメルケースを使う。
（例：`functionName`, `DataType`）
<!--original
Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.
-->

可読性のために、省略語も全て大文字にはしない。
例えば、`HTTPServer`ではなく`HttpServer`とする。
2文字の省略語は例外とする。例：`IO`
<!--original
For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.
-->

### モジュール
<!--original
### Modules
-->

モジュール名は単数形でつける。
例：
`Data.Map`とし、`Data.Maps`としない。
`Data.ByteString.Internal`とし、`Data.ByteString.Internals`としない。
<!--original
Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.
-->

遅延評価の扱い
--------------
<!--original
Dealing with laziness
---------------------
-->

他に理由がない場合、データ型は正格に、関数は遅延評価にする。
<!--original
By default, use strict data types and lazy functions.
-->

### データ型
<!--original
### Data types
-->

構成子のフィールドは、遅延にする明確な理由がない限り正格にするべきである。
これは遅延の過多に起因する多くのよくある過ちを避け、
プログラマが評価順序について悩む時間を減らす。
<!--original
Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.
-->

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

さらに、単純なフィールドをunpackすると性能は向上しメモリ使用量は減少することが多い。
<!--original
Additionally, unpacking simple fields often improves performance and
reduces memory usage:
-->

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

`UNPACK`プラグマの代わりに、ファイルの先頭で次のようにしてもよい。
<!--original
As an alternative to the `UNPACK` pragma, you can put
-->

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

このフラグをファイルそのものに入れることは、
他の方法、例えばCabalファイルに入れる方法より好ましい。
誰かが他の方法でそのファイルをコンパイルした場合でも最適化が適用されるからである。
（つまり、最適化指示がソースコードそのものに付いている。）
<!--original
at the top of the file. Including this flag in the file inself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).
-->

`-funbox-strict-fields`は小さなフィールド（例：`Double`や`Int`）だけでなく、
あらゆる正格なフィールドに適用されることに注意せよ。
GHC 7.4以降であれば、`-funbox-strict-fields`により適用されるunpackを
`NOUNPACK`により選択的に取り消すことができる。
<!--original
Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`). If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.
-->

### 関数
<!--original
### Functions
-->

関数の引数は正格にする明確な必要性がない限り遅延評価にする。
<!--original
Have function arguments be lazy unless you explicitly need them to be
strict.
-->

明確に正格引数が必要なよくある場合とは、蓄積引数を伴う再帰である。
<!--original
The most common case when you need strict function arguments is in
recursion with an accumulator:
-->

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

その他
------
<!--original
Misc
----
-->

### ポイントフリースタイル ###
<!--original
### Point-free style ###
-->

ポイントフリースタイルは過剰にしない。
例えば、以下は読みにくい。
<!--original
Avoid over-using point-free style. For example, this is hard to read:
-->

```haskell
-- Bad:
f = (g .) . h
```

### コンパイラによる警告 ###
<!--original
### Warnings ###
-->

コードは`-Wall -Werror`を付けてコンパイルできるべきである。
警告は出てはいけない。
<!--original
Code should be compilable with `-Wall -Werror`. There should be no
warnings.
-->

<script src="http://code.jquery.com/jquery-1.11.0.min.js"></script>
<script>
$(function() {
  $("*").contents().filter(function() {
    return this.nodeType==8 && this.nodeValue.match(/^original/);
  }).each(function(i, e) {
    var tooltips = e.nodeValue.replace(/^original *[\n\r]|[\n\r]$/g, '');
    var link = "<span><a href='#' onclick='javascript:return false;' class='toggleLink'>" + "*" + "</a></span>";
    $(this).prev().append(link);
    $(this).prev().after("<pre style='display:none'>"+ tooltips + "</pre>");
  });

  $('.toggleLink').click(
    function() {
      if ($(this).text()=="*") {
       $(this).parent().parent().next('pre').slideDown(200);
       $(this).text("hide");
      } else {
        $(this).parent().parent().next('pre').slideUp(200);
        $(this).text("*");
      };
    });
});
</script>
