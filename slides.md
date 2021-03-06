% Round tripping balls
% (with partial isomorphisms & Haskell)

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

Writing isomorphic round-trip printer/parsers with the get/put idiom is redundant and
error prone.

[/alert]

# We can have either bouncy or lumpy balls
```haskell
data Ball
    = Lumpy  { _colour     :: Text
             , _lumps      :: [[Bool]]
             }
    | Bouncy { _bouncyness :: Double }
  deriving (Eq, Show)
```
# Bouncy balls are happy, lumpy ones are not.

```haskell
[ Lumpy  {_colour = "Rainbow"
         , _lumps = [[True,False],[False,False]]}
, Bouncy {_bouncyness = 3.141592653589793}]
```
\center{\arrowdown}

```javascript
[{
      "colour" : "Rainbow",
      ":D"     : false,
      "lumps"  : [[true,false],[false,false]]
 }
,{
      "bouncyness" : 3.14159265358979,
      ":D"         : true
 }]
```

# We want to parse (partial)

```haskell
instance FromJSON Ball where
  parseJSON :: Value -> Parser Ball
  parseJSON (Object o) = do
    happy_ball <- o .: ":D"
    if happy_ball then parseBouncy else parseLumpy
   where
    parseBouncy =
        Bouncy <$> o .: "bouncyness"
    parseLumpy =
        Lumpy <$> o .: "colour" <*> o .: "lumps"
```

# And we want to print.

```haskell
instance ToJSON Ball where
  toJSON :: Ball -> Value
  toJSON (Lumpy colour lump_map) =
      object [ ":D"     .= True
             , "colour" .= colour
             , "lumps"  .= lump_map
             ]
  toJSON (Bouncy bouncyness) =
      object [ ":D"         .= True
             , "bouncyness" .= bouncyness
             ]
```

# What are we fixing?

[alert=Duplicate information!]

Potential errors and good programmers *HATE* typing

[/alert]


# Why don't you...

* Just write some tests

# Why don't you...

* \sout{Just write some tests} \alert{Unnecessary boilerplate.}
* Stop whining and trust the libraries

# Why don't you...

* \sout{Just write some tests} \alert{Unnecessary boilerplate.}
* Stop whining and trust the libraries

# Why don't you...

* \sout{Just write some tests} \alert{Unnecessary boilerplate.}
* \sout{Stop whining and trust the libraries} \alert{Too flexible.}

# Why don't you...

* \sout{Just write some tests} \alert{Unnecessary boilerplate.}
* \sout{Stop whining and trust the libraries} \alert{Too flexible.}
* Use template haskell/generics

# Why don't you...

* \sout{Just write some tests} \alert{Unnecessary boilerplate.}
* \sout{Stop whining and trust the libraries} \alert{Too flexible.}
* \sout{Use template haskell/generics} \alert{Not flexible enough.}

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

. . .

```json
"12.2012"
```

. . .


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

# Given a datatype:
```haskell
data List a
  = Nil
  | Cons a (List a)
```

# We define a Printer$^1$ combinator

```haskell
type Printer a = a -> Doc

printMany :: Printer a -> Printer (List a)
printMany p list
  = case list of
    Nil       -> text ""
    Cons x xs -> p x
              <> printMany p xs
```

# And a Parser$^2$ combinator

```haskell
newtype Parser a = Parser (String -> [(a, String)])

parseMany :: Parser a -> Parser (List a)
parseMany p
  =  const Nil <$> text ""
 <|> Cons      <$> p
               <*> parseMany p
```

# It would be nice if...

```haskell
combined :: Unicorn x => x a -> x (List a)
combined p
  =  magic Nil <$> fairies ""
 <|> Cons      <$> p
               <*> parseMany p
```

# co/contravariance

```haskell
newtype Parser a = Parser (String -> [(a, String)])

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> Parser p = Parser $ (fmap . first) f . p
```

. . .


```haskell
type Printer a = a -> Doc

(<$>) :: (a -> b) -> Printer a -> Printer b
```

. . .

Can you implement this? \arrowup

# co/contravariance

Covariant  \arrowdown


```haskell
newtype Parser a = Parser (String -> [(a, String)])

(<$>) :: (a -> b) -> Parser a -> Parser b
```


```haskell
type Printer a = a -> Doc

(<$>) :: (b -> a) -> Printer a -> Printer b
```


Contravariant \arrowup

# Partial Iso$^3$ (simplified)

```haskell
data Iso a b = Iso
    { apply   :: a -> Maybe b
    , unapply :: b -> Maybe a
    }
```

# co/contravariance (revisited)

```haskell
newtype Parser a = Parser (String -> [(a, String)])

(<$>) :: (a -> b) -> Parser a -> Parser b
```

. . .


```haskell
type Printer a = a -> Doc

(<$>) :: (b -> a) -> Printer a -> Printer b
```

. . .

[block=The solution: IsoFunctor\$^4\$ ]

```haskell
class IsoFunctor f where
  (<$>) :: Iso a b -> f a -> f b
```

[/block]

# The important things about partial isos

* Unifying a functor requires both $a \to b$ and $b \to a$

. . .

* We unify both with a partial Iso, where these functions can fail

. . .

* We defined IsoFunctor (from partial isos to printer/parsers)

# Applicative

```haskell
(<*>) :: f (a -> b) -> f a -> f b

instance Applicative Parser where
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
```

. . .


```haskell
class UnhelpfulIsoApplicative where
  (<*>) :: f (Iso a b) -> f a -> f b
```

. . .

```haskell
type Printer a = a -> Doc

instance UnhelpfulIsoApplicative Printer where
  (<*>) :: (Iso a b -> Doc) -> (a -> Doc) -> b -> Doc
  (f <*> g) b = error "how do I shot web?"
```

# Applicative

```haskell
class Functor f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b
```

. . .

```haskell
class ProductFunctor f where
  infixr 6 <*>
  (<*>) :: f a -> f b -> f (a, b)
```

# Applicative example

```haskell
f :: Applicative f
  => (a -> b -> c -> d)
  -> f a -> f b -> f c -> f d
f ctor fa fb fc =   ctor <$> fa  <*> fb  <*> fc
f ctor fa fb fc = ((ctor <$> fa) <*> fb) <*> fc
```

. . .

\center{\arrowdown}

```haskell
f :: (ProductFunctor f, IsoFunctor f)
  => Iso (a, (b, c)) d
  -> f a -> f b -> f c -> f d
f ctor fa fb fc = ctor <$>  fa <*> fb  <*> fc
f ctor fa fb fc = ctor <$> (fa <*> (fb <*> fc))
```

# We tuple tree isos for our data types

```haskell
nil  :: Iso ()          (List a)
cons :: Iso (a, List a) (List a)
```

. . .
\center{\arrowup}

```haskell
data List a
  = Nil
  | Cons a (List a)

defineIsomorphisms ''List
```

# The important things: ProductFunctor

. . .

* Naively adapting Applicative leaves us with an uninhabitable type.

. . .

* We use ProductFunctor, it has tuples instead of currying and associates
  right

. . .

* <*> mushes tuples together one way, and takes them apart the other

# Almost done, Alternative$^6$ is trivial

```haskell
class Alternative where
  (<|>) :: f a -> f a -> f a
```

# We now have an abstract Syntax$^7$
```haskell

class (IsoFunctor s, ProductFunctor s, Alternative s)
       => Syntax s where
  pure :: Eq a => a -> s a
```

# Invertible Syntax Descriptions: the punchline

```haskell
parseMany :: Parser a -> Parser (List a)
parseMany p
  =  const Nil <$> text ""
 <|> Cons      <$> p

printMany :: (a -> Doc) -> (List a -> Doc)
printMany p list
  = case list of
    Nil       -> text ""
    Cons x xs -> p x
              <> printMany p xs
```

# Invertible Syntax Descriptions: the punchline

```haskell
many :: Syntax s => s a -> s (List a)
many p
  =  nil  <$> pure ()
 <|> cons <$> p <*> many p
```

# The implementation of Syntax for Printer

```haskell
instance IsoFunctor Printer where
  iso <$> Printer p
    = Printer (\b -> unapply iso b >>= p)

instance ProductFunctor Printer where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q = Printer (\s -> mplus (p s) (q s))

instance Syntax Printer where
  pure x
    = Printer (\y -> if x == y then Just "" else N...)
```

# Invertible Syntax Descriptions: summary

. . .

* Partial isos: composable building blocks for munging data

. . .

* IsoFunctor: to "lift" theses isos into concrete printers or parsers

. . .

* ProductFunctor: to handle multiple fields and recursion via tuples

. . .

* Syntax: to glue all these constraints together and add pure

# Let's try it on enterprise JSON!

![We are now enterprise developers](dog.jpg)

# Two primitives for all your JSON needs:

```haskell
class Syntax s => JsonSyntax s where
    runSub :: s v -> s Value -> s v

    value :: s Value
```

# JsonBuilder/Parser IsoFunctor, simple!
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
# JsonBuilder ProductFunctor: Mush tuples together

```haskell
instance ProductFunctor JsonBuilder where
  (<*>) :: JsonBuilder a -> JsonBuilder b -> JsonBuilder (a,b)
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

# JsonParser ProductFunctor: Tuple the things

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

# JsonBuilder/Parser Alternative: Try one, then the other

```haskell
instance Alternative JsonBuilder where
  (<||>) :: JsonBuilder a -> JsonBuilder a -> JsonBuilder a
  JsonBuilder p <||> JsonBuilder q =
    JsonBuilder $ \a -> p a `mplus` q a

  empty :: JsonBuilder a
  empty = JsonBuilder $ const Nothing

instance Alternative JsonParser where
  (<||>) :: JsonParser a -> JsonParser a -> JsonParser a
  JsonParser p <||> JsonParser q =
    JsonParser $ \v -> p v `mplus` q v

  empty :: JsonParser a
  empty = JsonParser $ const Nothing
```


# JsonBuilder/Parser JsonSyntax: Providing access to Values

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

The lens-aeson package provides primitives for most of the combinators we want,
but it has these \sout{terrifying} powerful Prism things:

```haskell
type Prism s t a b =
    (Choice p, Applicative f) =>
      p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a
```

. . .


```haskell
preview :: Prism' a b -> a -> Maybe b
review  :: Prism' a b -> b -> a
```

# Prisms/isos are "stronger" than partial isos

```haskell
demote :: Prism' a b -> Iso a b
demote p = unsafeMakeIso (preview p)
                         (review (_Just . p))
```

# JsonSyntax combinators

```haskell
_Bool :: Prism' Value Bool
```

. . . 

```haskell
demote _Bool :: Iso Value Bool
value :: Syntax s => s Value
```

. . .


```haskell
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

# Combinators: Looking up keys in objects

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

# Combinators: When you want to ensure something is there

```haskell
is :: (JsonSyntax s, Eq a) => s a -> a -> s ()
is s a = demote (prism' (const a) 
                        (guard . (a ==))) <$> s
```

# JsonSyntax summary

. . .

* JsonSyntax: to provide access to the underlying domain specific data

. . .

* Prisms are stronger than partial isos

. . .

* lens-aeson made it easy to define JSON combinators

. . .

* These combinators can be thought of as relations between abstract syntax and
  concrete syntax.

# Example - Round-tripping balls

![A happy ball](tripping.jpg)

# We can have either bouncy or lumpy balls
```haskell
data Ball
    = Lumpy  { _colour     :: Text
             , _lumps      :: [[Bool]]
             }
    | Bouncy { _bouncyness :: Double }
  deriving (Eq, Show)
```

# Bouncy balls are happy, lumpy ones are not.

```haskell
[ Lumpy  {_colour = "Rainbow"
         , _lumps = [[True,False],[False,False]]}
, Bouncy {_bouncyness = 3.141592653589793}]
```
\center{\arrowdown}

```javascript
[
   {
      "colour" : "Rainbow",
      ":D" : false,
      "lumps" : [[true,false],[false,false]]
   },
   {
      "bouncyness" : 3.14159265358979,
      ":D" : true
   }
]
```

# Ball syntax

```haskell
ballSyntax :: JsonSyntax s => s Ball
ballSyntax
  =  lumpy <$> jsonField ":D" (jsonBool `is` False)
            *> jsonField "colour" jsonString
           <*> jsonField "lumps" (many $ many jsonBool)
 <|> bouncy
           <$> jsonField ":D" (jsonBool `is` True)
            *> jsonField "bouncyness" jsonRealFloat
```

# The test

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

# Output (whitespace added)

```javascript
[  {
      "colour" : "Rainbow",
      ":D" : false,
      "lumps" : [[true,false],[false,false]]
   },
   {
      "bouncyness" : 3.14159265358979,
      ":D" : true
}  ]

[ Lumpy "Rainbow" [[True,False],[False,False]]
, Bouncy 3.141592653589793 ]

True
```

# Real world example (currency)

```haskell
-- | Parse an enterprise currency field, which is a
--   blob of text looking like:
--
--   "00.43"
currency :: JsonSyntax s => s Scientific
currency = demoteLR "currency" (prism' f g) <$> value
  where
    f = String . LT.toStrict . LT.toLazyText . fmt
    g = readMaybe . ST.unpack
      . ST.filter (not . isSpace) <=< preview _String
    -- We render with arbitrary precision (Nothing)
    -- and standard decimal notation (Fixed)
    fmt = LT.formatScientificBuilder Fixed Nothing
```

# Real world example (datetime)

```haskell
-- | Parse an enterprise datetime field, looks like:
--
--    "20/6/2014 4:25 pm"
datetime :: JsonSyntax s => s UTCTime
datetime = demoteLR "datetime" (prism' f g) <$> value
  where
    f = String . ST.pack . opts formatTime
    g = opts parseTime . ST.unpack <=< preview _String
    opts h =
      h defaultTimeLocale "%-d/%-m/%Y %-l:%M %P"
```

# Summary/Conclusion

. . .

* Invertible syntax descriptions are a way to write better printer/parsers such
  that we \alert{type less}, \alert{think less} and make \alert{fewer
  mistakes}.

. . .

* Problem: writing round-trip printer/parsers with the get/put idiom is
  \alert{redundant and error prone}.

. . .

* Generics/TH \alert{too rigid}, libraries \alert{too flexible}.

. . .

* A reasonable middle ground is detailed in the paper by \alert{Tillmann Rendel
  and Klaus Ostermann}. \alert{Invertible Syntax Descriptions}: Unifying
  Parsing and Pretty Printing.

. . .

* I'd love to work with people on improving the current machinery.

. . .

* Our library: http://github.com/anchor/roundtrip-aeson

# A note on categories and type classes

```haskell
import qualified Control.Categorical.Functor as CF

type Hask = (->)

instance CF.Functor JsonBuilder Iso Hask where
    fmap iso (JsonBuilder f) =
        JsonBuilder (unapply iso >=> f)
```
