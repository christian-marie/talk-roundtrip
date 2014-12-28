% Error-free JSON round-trips
% (half the typing, partial isomorphisms & Haskell)

# What do I want to show you?

\fontsize{14}{14}\selectfont

How to write better printer/parsers such that we \alert{type less},
\alert{think less} and make \alert{fewer mistakes}.


# Outline

. . .

1. Define problem

. . .

2. Summarise paper

. . .

3. Build your own

# What are we fixing?

[alert=The problem]

Writing round-trip printer/parsers with the get/put idiom is redundant and
error prone.

[/alert]

# What are we fixing?

## Given a datatype:
```haskell
data ReadRequest
    = SimpleReadRequest   Address Time Time
    | ExtendedReadRequest Address Time Time
  deriving (Eq, Show)
```

# What are we fixing?

## Get/put, look familiar?
```haskell
class WireFormat a where
    fromWire :: ByteString -> Either SomeException a
    toWire   :: a -> ByteString
```

# What are we fixing?

## Encoding (total)
```haskell

instance WireFormat ReadRequest where
    toWire (SimpleReadRequest addr start end) =
        packWithHeaderByte 0 addr start end
    toWire (ExtendedReadRequest addr start end) =
        packWithHeaderByte 1 addr start end

packWithHeaderByte :: Word8 -> Address -> Time -> Time
                   -> ByteString
packWithHeaderByte header addr start end =
    let addr_bytes = toWire addr
    in runPacking 25 $ do
        putWord8 header
        putBytes addr_bytes
        putWord64LE start
        putWord64LE end
```
# What are we fixing?

## Decoding (partial)
```haskell
fromWire bs
    | S.length bs /= 25 =
        Left . SomeException $ userError "/= 25 bytes"
    | otherwise = flip tryUnpacking bs $ do
        header <- getWord8
        addr_bytes <- getBytes 8
        addr <- either (fail . show) return $
            fromWire addr_bytes
        start <- getWord64LE
        end <- getWord64LE
        case header of
            0 -> return $
                    SimpleReadRequest addr start end
            1 -> return $
                    ExtendedReadRequest addr start end
            _ -> fail "invalid header byte"
```

# What are we fixing?

## So, we test it and hope for the best:

```haskell
suite :: Spec
suite = describe "WireFormat identity tests" $ do
    prop "ReadRequest" (wireId :: ReadRequest -> Bool)
    prop "ReadStream"  (wireId :: ReadStream -> Bool)

wireId :: (Eq w, WireFormat w) => w -> Bool
wireId op = id' op == op
  where
    id' = fromRight . fromWire . toWire
    fromRight = either (error . show) id
```

# Some proposed solutions

# Some proposed solutions

* Stop whining and trust the libraries

# Some proposed solutions

* \sout{Stop whining and trust the libraries} \alert{Too flexible.}

# Some proposed solutions

* \sout{Stop whining and trust the libraries} \alert{Too flexible.}
* Template haskell/Generics

# Some proposed solutions

* \sout{Stop whining and trust the libraries} \alert{Too flexible.}
* \sout{Template haskell/Generics} \alert{Not flexible enough.}

# Introducing: Enterprise JSON

. . .

## "datetime"
```json
"27/6/2013 10:29 pm"
```

. . .

## "date"

```json
"12/12/2012"
```
. . .

## "mmdddate"

```json
"12/12"
```
. . .

## "mmyydate"

```json
"12/2012"
```

OR

```json
"12.2012"
```

OR

```json
"122012"
```

# Introducing: Enterprise JSON

## "checkbox"

```json
"T"

```
. . .

## "currency", "currency2" and "poscurrency"

```json
"00.03"
```
. . .

## "posfloat", "nonnegfloat"

```json
"2.718281828459045"
```

# Introducing: Enterprise JSON

![Concern for sanity](concerned.png)

# A wild paper appears!

![](paper.png)\newline

# Invertible Syntax Descriptions: way of the get/put

## Given a datatype:
```haskell
data List a
  = Nil
  | Cons a (List a)
```

# Invertible Syntax Descriptions: way of the get/put

## Printing$^5$

```haskell
type Printer a = a -> Doc

printMany :: Printer a -> Printer (List a)
printMany p list
  = case list of
    Nil       -> text ""
    Cons x xs -> p x
              <> printMany p xs
```

. . .

## Parsing$^6$
```haskell
parseMany :: Parser a -> Parser (List a)
parseMany p
  =  const Nil <$> text ""
 <|> Cons      <$> p
               <*> parseMany p
```

# Invertible Syntax Descriptions: way of the get/put

## It would be nice if...

```haskell
combined :: Unicorn x => x a -> x (List a)
combined p
  =  magic Nil <$> faries ""
 <|> Cons      <$> p
               <*> parseMany p
```

# Invertible Syntax Descriptions: co/contravariance

[block=Parser fmap]

```haskell
newtype Parser a = Parser (String -> [(a, String)])

(<$>) :: (a -> b) -> Parser a -> Parser b
```

[/block]

. . .

[alert=Printer fmap]

```haskell
type Printer a = a -> Doc

(<$>) :: (b -> a) -> Printer a -> Printer b
```

[/alert]

# Invertible Syntax Descriptions: co/contravariance

## Partial isomorphisms for typists
```haskell
-- Not the same as a Control.Lens Iso
data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

inverse :: Iso a b -> Iso b a
inverse (Iso f g) = Iso g f

apply :: Iso a b -> a -> Maybe b
apply (Iso f g) = f

unapply :: Iso a b -> b -> Maybe a
unapply = apply . inverse

instance Category Iso where
    g . f = Iso (apply f   >=> apply g)
                (unapply g >=> unapply f)
    id = Iso Just Just
```

# Invertible Syntax Descriptions: co/contravariance
## Partial Iso$^1$ (academia decoded)

```haskell
data Iso a b = Iso
    { apply   :: a -> Maybe b
    , unapply :: b -> Maybe a
    }
```

# Invertible Syntax Descriptions: co/contravariance

[block=Parser fmap]

```haskell
newtype Parser a = Parser (String -> [(a, String)])

(<$>) :: (a -> b) -> Parser a -> Parser b
```

[/block]

. . .

[alert=Printer fmap]

```haskell
type Printer a = a -> Doc

(<$>) :: (b -> a) -> Printer a -> Printer b
```

[/alert]

. . .

[block=The solution: IsoFunctor\$^2\$ ]

```haskell
class IsoFunctor f where
  (<$>) :: Iso a b -> f a -> f b
```

[/block]

# Invertible Syntax Descriptions: applicative

[block=Normal applicative]

```haskell
(<*>) :: f (a -> b) -> f a -> f  b

instance Applicative Parser where
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
```

[/block]

. . .

[block=Adapting that directly]

```haskell
class UnhelpfulIsoApplicative where
  (<*>) :: f (Iso a b) -> f a -> f b
```

[/block]

. . .

[alert=Falls apart on Printer (the contravariant one)]

```haskell
type Printer a = a -> Doc

instance Applicative Printer where
  (<*>) :: (Iso a b -> Doc) -> (a -> Doc) -> b -> Doc
  (f <*> g) b = error "impossible!"
```

[/alert]

# Invertible Syntax Descriptions: applicative

## Normal applicative
```haskell
class Functor f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b
```

. . .

## *#!@ it, associate right and tuple (ProductFunctor$^3$)

```haskell
class ProductFunctor f where
  infixr 6 <*>
  (<*>) :: f a -> f b -> f (a, b)
```

# Invertible Syntax Descriptions: applicative

## Normal (currying applicative, left associative)

```haskell
f :: Applicative f
  => (a -> b -> c -> d)
  -> f a -> f b -> f c -> f d
f ctor fa fb fc =   ctor <$> fa  <*> fb  <*> fc
f ctor fa fb fc = ((ctor <$> fa) <*> fb) <*> fc
```

. . .

## Our new, alternate universe

```haskell
f :: (ProductFunctor f, IsoFunctor f)
  => Iso (a, (b, c)) d
  -> f a -> f b -> f c -> f d
f ctor fa fb fc = ctor <$>  fa <*> fb  <*> fc
f ctor fa fb fc = ctor <$> (fa <*> (fb <*> fc))
```

# Invertible Syntax Descriptions: applicative

## We want these tuple tree isos for our data types

```haskell
nil  :: Iso ()          (List a)
cons :: Iso (a, List a) (List a)
```

. . .

## So we magic them:
```haskell
data List a
  = Nil
  | Cons a (List a)

defineIsomorphisms ''List
```

# Invertible Syntax Descriptions: alternative

## Alternative$^4$ is trivial
```haskell
class Alternative where
  (<|>) :: f a -> f a -> f a
```

## And we now have an abstract Syntax$^5$
```haskell

class (IsoFunctor s, ProductFunctor s, Alternative s)
       => Syntax s where
  pure :: Eq a => a -> s a
```

# Invertible Syntax Descriptions: the punchline

## Parsing
```haskell
parseMany :: Parser a → Parser (List a)
parseMany p
  =  const Nil <$> text ""
 <|> Cons      <$> p
               <*> parseMany p
```

## Printing
```haskell
printMany :: (a -> Doc) -> (List a -> Doc)
printMany p list
  = case list of
    Nil       -> text ""
    Cons x xs -> p x
              <> printMany p xs
```

# Invertible Syntax Descriptions: the punchline

## Invertible many
```haskell
many :: Syntax s => s a -> s (List a)
many p
  =  nil  <$> pure ()
 <|> cons <$> p <*> many p
```

# Invertible Syntax Descriptions: printer syntax

## The implementation of Syntax for Printer
```haskell
instance IsoFunctor Printer where
  iso <$> Printer p
    = Printer (\b -> unapply iso b >>= p)

instance ProductFunctor Printer where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q
    = Printer (\s -> mplus (p s) (q s))

instance Syntax Printer where
  pure x
    = Printer (\y -> if x == y then Just "" else N...)
```

# Let's try it on enterprise JSON!

![Why must enterprise hurt us so?](sparta.jpg)

# Let's try it on enterprise JSON!

## Two primitives for all your JSON needs:

```haskell
class Syntax s => JsonSyntax s where
    runSub :: s v -> s Value -> s v

    value :: s Value
```

# JsonBuilder/Parser IsoFunctor

## Starts off simple
``` haskell
newtype JsonBuilder a = JsonBuilder
  { runBuilder :: a -> Maybe Value }

newtype JsonParser a = JsonParser
  { runParser :: Value -> Maybe a }

instance IsoFunctor JsonBuilder where
  (<$>) :: Iso a b -> JsonBuilder a -> JsonBuilder b
  i <$> JsonBuilder b = JsonBuilder $ unapply i >=> b

instance IsoFunctor JsonParser where
  (<$>) :: Iso a b -> JsonParser a -> JsonParser b
  i <$> JsonParser p = JsonParser $ apply i <=< p
```
# JsonBuilder ProductFunctor

## Mush tuples together with applicative when building

```haskell
instance ProductFunctor JsonBuilder where
  (<*>) :: JsonBuilder a
        -> JsonBuilder b
        -> JsonBuilder (a,b)
  JsonBuilder p <*> JsonBuilder q =
    JsonBuilder $ \(a,b) -> do
      a' <- p a
      b' <- q b
      merge a' b'
    where
      merge (Object a) (Object b) =
      	Just . Object $ a `union` b
      merge a (Array b) = Just . Array $ V.cons a b
      merge x Null = Just x
      merge Null x = Just x
      merge _ _ = Nothing
```

# JsonParser ProductFunctor

## Take the things apart and tuple them when parsing
```haskell
instance ProductFunctor JsonParser where
  (<*>) :: JsonParser a -> JsonParser b -> JsonParser (a,b)
  JsonParser p <*> JsonParser q =
    JsonParser $ \v -> do
      let (a,b) | Array x <- v, Just y <- x !? 0
                = (y, Array $ V.tail x)
                | Array _ <- v
       	        = (Null, Null)
                | otherwise
      	        = (v,v)
      liftM2 (,) (p a) (q b)
```

# JsonBuilder/Parser Alternative


## Try one, otherwise the other. Same implementation.
```haskell
instance Alternative JsonBuilder where
  (<||>) :: JsonParser a -> JsonParser a -> JsonParser a
  JsonBuilder p <||> JsonBuilder q =
    JsonBuilder $ \a -> p a `mplus` q a

  empty :: JsonParser a
  empty = JsonBuilder $ const Nothing

instance Alternative JsonParser where
  (<||>) :: JsonParser a -> JsonParser a -> JsonParser a
  JsonParser p <||> JsonParser q =
    JsonParser $ \v -> p v `mplus` q v

  empty :: JsonParser a
  empty = JsonParser $ const Nothing
```


# JsonBuilder/Parser JsonSyntax
## Try one maybe, otherwise take the other
```haskell
instance JsonSyntax JsonBuilder where
  value :: JsonBuilder Value
  value = JsonBuilder Just

  runSub :: JsonBuilder v
         -> JsonBuilder Value
         -> JsonBuilder v
  runSub (JsonBuilder a) (JsonBuilder b) =
    JsonBuilder $ a >=> b

instance JsonSyntax JsonParser where
  value = JsonParser Just

  runSub (JsonParser a) (JsonParser b) =
    JsonParser $ a <=< b
```

# JsonSyntax combinators

## Review/preview
```haskell
preview :: Prism' a b -> a -> Maybe b
review  :: Prism' a b -> b -> a
```
# Prisms/isos are "stronger" than partial isos

## Demoting prisms and "real" isos
```haskell
demote :: Prism' a b -> Iso a b
demote p = unsafeMakeIso (preview p)
                         (review (_Just . p))
```

# JsonSyntax combinators

## Combinators come together

```haskell
_Bool :: Prism' Value Bool

demote _Bool :: Iso Value Bool

value :: s Value

(<$>) :: Iso Value Bool -> s Value -> s Bool


jsonBool :: JsonSyntax s => s Bool
jsonBool = demote _Bool <$> value

```

. . .

```haskell
jsonNumber :: JsonSyntax s => s Scientific
jsonNumber = demote _Number <$> value

jsonString :: JsonSyntax s => s Text
jsonString = demote _String <$> value
```

# JsonSyntax combinators

## Looking up keys in objects

```haskell
runSub :: s v -> s Value -> s v

jsonField
    :: JsonSyntax s
    => Text
    -- ^ Key to lookup/insert
    -> s v
    -- ^ Sub-parser
    -> s v
jsonField k syntax = runSub syntax (keyIso <$> value)
  where
    keyIso = demote $ prism' (\x -> Object [(k,x)])
                             (^? key k)
```

# JsonSyntax combinators

## When you want to ensure something is there

```haskell
is :: (JsonSyntax s, Eq a) => s a -> a -> s ()
is s a = demote (prism' (const a) 
                (guard . (a ==))) <$> s
```

# JsonSyntax combinators

![](combinators.png)

# Example

## We wish to produce and parse:

```json
[{ "colour" : "Rainbow"
 , "lumps"  : [[true,false]
              ,[false,false]]
 , ":D"     : false
 },
 { ":D"     : true,
   "bouncyness" : 3.14159265358979
 }]
```

# Example

## Round tripping balls

```haskell
data Ball
    = Lumpy Text [[Bool]]
    | Bouncy Double deriving (Eq, Show)
defineIsomorphisms ''Ball

ballSyntax :: JsonSyntax s => s Ball
ballSyntax
  =  lumpy <$> jsonField ":D" (jsonBool `is` False)
            *> jsonField "colour" jsonString
           <*> jsonField "xss" (many $ many jsonBool)
 <|> bouncy
           <$> jsonField ":D" (jsonBool `is` True)
            *> jsonField "bouncyness" jsonRealFloat
```

# Example

## The test

```haskell
main :: IO ()
main = do
    let tony  = Lumpy "Rainbow"
                      [[True, False], [False, False]]
    let oscar = Bouncy pi
    let pit   = [tony, oscar]

    let Just blob = runBuilder (many ballSyntax) pit
    L.putStrLn $ encode blob

    let Just pit' = runParser (many ballSyntax) blob
    print pit'
    print $ pit == pit'
```

# Examples

## Output (whitespace added)

```json
[{ "colour" : "Rainbow"
 , "lumps"  : [[true,false]
              ,[false,false]]
 , ":D"     : false
 },
 { ":D"     : true,
   "bouncyness" : 3.14159265358979
 }]

[ Lumpy "Rainbow" [[True,False],[False,False]]
, Bouncy 3.141592653589793
]

True
```
