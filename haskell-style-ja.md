Haskell �X�^�C���K�C�h
======================

https://github.com/tibbe/haskell-style-guide �̖|��ł���B
�����ƌ`���Ɋւ���L���͈͂Ɍ��y�����B
�{�K�C�h�Ō��y����Ă��Ȃ������Ɋւ��ẮA���̃��W���[���̃R�[�h�ɍ��킹�邱�ƁB

�`��
----

### �s�̒���

1�s��*80����*�ȉ��Ƃ���B

### ������

�^�u�͎g��Ȃ��B�������ɂ͋󔒕�����p����B
�u���b�N��*4����*������B
�\��� `where` �͖{�̂���2���������A���̒�`���e�͂����2����������B�ȉ��ɗ�������B

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

(*JOE 4���Ƒ傫���̂�2�Awhere�͑O�̍s�Ƃ����s�ߖ�X�^�C�����D�݂����A
where��1�s�Ƃ�����s�V�͂����̂��낤�ȁB*)

### ��s

�g�b�v���x����`�͋�s1�s�ŋ�؂�B
�^�V�O�l�`���Ɗ֐���`�̊Ԃɂ͋�s�����Ȃ��B
�^�N���X�̃C���X�^���X�錾�Ɗ֐��̊Ԃ́A�֐��{�̂��傫���ꍇ�ɂ�1�s�J����B(���ȍٗ�)

### ��

�񍀉��Z�q�̗����ɂ͋󔒂�1�����J����B
�Z�p���Z�q�̎���̋󔒂͎��Ȕ��f�ł悢���A�񍀉��Z�q�̗����̋󔒂ɂ͈�ѐ�����������B
�ɂ̌�ɂ͋󔒂����Ȃ��B

(*JOE ���̎Z�p���Z���{�����A�܂��̓��[�v�ϐ���increment�̂悤�ȕ⏕���ŁA
(n - k) �Ƃ����� (n-1) �Ƃ����菑���������肷��̂��D�݂�����ǁA
��ł��̃R�[�h��ێ炷��l�ɂƂ��Ă̓E�U�������Ȃ񂾂낤���B*)

### �f�[�^�^�錾

�f�[�^�^�錾�̍\���q�͈ʒu��������B��F

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

�^���������Ƃ��́A�ȉ��̌`�����������B

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

���R�[�h�����l�ɂ���F

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### ���X�g��`

���X�g�̗v�f������������B��F

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

���߂ɉ��s���Ȃ����@������B���ȍٗʁB

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### �v���O�}

�v���O�}�͓K�p����֐��̒���ɒu���B��F

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

�f�[�^�^�錾�̏ꍇ�́A�K�p����^�̒��O�ɒu���B��F

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### �Ԃ牺����� (Hanging Lambdas)

�u�Ԃ牺����v�ɂ́A���������Ă����Ȃ��Ă��悢�B���ȍٗʁB��������B

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

### �A�o�ꗗ

�A�o�ꗗ�͎��̂悤�ɐ��`����B

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

### if-then-else ��

�S�ʓI�ɁA�\�Ȃ�K�[�h�ƃp�^�[���}�b�`��if�߂��]�܂����B
�Z���ꍇ������1�s�ɓ����i�s�̒����Ɏ��܂�Ȃ�΁j�B

���i�h��p���Ȃ��i���Ȃ킿`do`���g��Ȃ��j�R�[�h�ŁA
�K�[�h�ƃp�^�[���}�b�`���g���Ȃ��ꍇ�́A
�ʏ�̎��ł������悤��if-then-else�߂����������Ă悢�B
(you can align if-then-else clauses you like you would normal expressions)

```haskell
foo = if ...
      then ...
      else ...
```

����ȊO�̏ꍇ��4���������K���ɏ]���A�\���`then`��`else`�͑�����B��F

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

do�u���b�N�̓���q�ɂ������K����K�p����F

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

(*JOE where��1�s�Ȃ̂�do�͑O�̍s�ɂԂ牺����́A��ѐ��Ȃ��Ȃ��H*)

### case ��

case���̑I�����͎���2�̌`���̂����ꂩ�ɐ��`����B

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

�܂���

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

�ǐ��̌���̂��߂�`->`�𑵂���B
(*JOE �����2-2�ɂ����Ƃ��������B*)

�A��
----

�A���͎��̏��ɕ��ނ���F

1. �W�����C�u�����̗A��
2. �֘A����T�[�h�p�[�e�B�̗A��
3. �A�v���P�[�V�����܂��̓��C�u�����ŗL�̗A��

�A���̊e���ނ͋�s�ŋ�؂�B
���ނ̒����ƂɃ��W���[�����ɂ��A���t�@�x�b�g���ɐ��񂳂���B
(* JOE �����܂�!? *)

�W�����C�u�����ƃT�[�h�p�[�e�B�̃��C�u�����ɂ��Ă͕K���A
�����I�ȗA���ꗗ��t���邩`qualified`�ɂ���B
����́A���C�u�������̕ω��ɑ΂��Ď����̃R�[�h���挒�ɂ���B
��O�FPrelude

����
----

(*JOE ���������A�p��ŏ����A���K�v���H�����Ȃ����B*)

### ��ǖ@

���������������B(�p��̏ꍇ��)�啶���Ŏn�߁A��Ǔ_�𐳂����g���B

(*JOE �֐�����capitalize���ăn�}���Ċy�����H*)

### �g�b�v���x����`

�S�Ẵg�b�v���x���֐��A���ɗA�o����֐��ɂ͒��߂�t���A�^�V�O�l�`����t����B
�R�����g��Haddock�L�@�ɏ]���B
�A�o����f�[�^�^�ɂ͒��߂�����B
�֐��̗�������B

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

�֐��ɑ΂�������́A�֐��̒�`�����Ȃ��Ă��֐������p�ł���悤�ɏ[���ȏ���^����B

���R�[�h�̗�F

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

�������߂��K�v�ȃt�B�[���h�͎��̂悤�ɐ��`����F

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

### �s������

�s�����߂ƃR�[�h�͋󔒂�2�����J����B
�f�[�^�^��`�̒��߂͌���������B
��������F

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### �����N

�C�����C�������N���ӂ񂾂�Ɏg���B
API���Ƀ����N��t���邱�Ƃ����シ��B
Haddock�R�����g���̑S�Ă�API���Ƀ����N��t����K�v�͂Ȃ��B
����āA���̏ꍇ�ɂ�������F

* �i��ϓI�Ɂj���p�҂����ۂɃN���b�N���ď��𓾂����炵���ӏ�

* �R�����g���ŊeAPI���������ŏ��̏ꏊ�̂݁i�d�����C�ɂ���K�v�͂Ȃ��j

(*JOE ��O���炯�����A��ϓI�ŃE�U���Ȃ��B*)

����
----

�֐������f�[�^�^�����A�L�������P�[�X���g���B
�i��F`functionName`, `DataType`�j

�ǐ��̂��߂ɁA�ȗ�����S�đ啶���ɂ͂��Ȃ��B
�Ⴆ�΁A`HTTPServer`�ł͂Ȃ�`HttpServer`�Ƃ���B
2�����̏ȗ���͗�O�Ƃ���B��F`IO`

(*JOE ����̓v���W�F�N�g���ɕς��Ă�������ˁB*)

### ���W���[��

���W���[�����͒P���`�ł���B
��F
`Data.Map`�Ƃ��A`Data.Maps`�Ƃ��Ȃ��B
`Data.ByteString.Internal`�Ƃ��A`Data.ByteString.Internals`�Ƃ��Ȃ��B

�x���]���̈���
--------------

���ɗ��R���Ȃ��ꍇ�A�f�[�^�^�͐��i�ɁA�֐��͒x���]���ɂ���B

### �f�[�^�^

�\���q�̃t�B�[���h�́A�x���ɂ��閾�m�ȗ��R���Ȃ����萳�i�ɂ���ׂ��ł���B
����͒x���̉ߑ��ɋN�����鑽���̂悭����߂�������A
�v���O���}���]�������ɂ��ĔY�ގ��Ԃ����炷�B

```haskell
-- �悢
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- ����
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

����ɁA�P���ȃt�B�[���h��unpack����Ɛ��\�͌��サ�������g�p�ʂ͌������邱�Ƃ������B

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

`UNPACK`�v���O�}�̑���ɁA�t�@�C���̐擪�Ŏ��̂悤�ɂ��Ă��悢�B

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

���̃t���O���t�@�C�����̂��̂ɓ���邱�Ƃ́A
���̕��@�A�Ⴆ��Cabal�t�@�C���ɓ������@���D�܂����B
�N�������̕��@�ł��̃t�@�C�����R���p�C�������ꍇ�ł��œK�����K�p����邩��ł���B
�i�܂�A�œK���w�����\�[�X�R�[�h���̂��̂ɕt���Ă���B�j

`-funbox-strict-fields`�͏����ȃt�B�[���h�i��F`Double`��`Int`�j�����łȂ��A
�����鐳�i�ȃt�B�[���h�ɓK�p����邱�Ƃɒ��ӂ���B
GHC 7.4�ȍ~�ł���΁A`-funbox-strict-fields`�ɂ��K�p�����unpack��
`NOUNPACK`�ɂ��I��I�Ɏ��������Ƃ��ł���B

### �֐�

�֐��̈����͐��i�ɂ��閾�m�ȕK�v�����Ȃ�����x���]���ɂ���B

���m�ɐ��i�������K�v�Ȃ悭����ꍇ�Ƃ́A�~�ψ����𔺂��ċA�ł���B

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

���̑�
------

### �|�C���g�t���[�X�^�C�� ###

�|�C���g�t���[�X�^�C���͉ߏ�ɂ��Ȃ��B
�Ⴆ�΁A�ȉ��͓ǂ݂ɂ����B

```haskell
-- ����
f = (g .) . h
```

### �R���p�C���ɂ��x�� ###

�R�[�h��`-Wall -Werror`��t���ăR���p�C���ł���ׂ��ł���B
�x���͏o�Ă͂����Ȃ��B
