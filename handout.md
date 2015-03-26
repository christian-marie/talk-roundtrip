---
title: Round tripping balls (with partial isomorphisms & Haskell)
author: Christian Marie \<christian@ponies.io\>
date: yay
header-includes:
    - \usepackage{fancyhdr}
    - \usepackage{eucal}
    - \pagestyle{fancy}
    - \renewcommand{\footrulewidth}{0.4pt}% default is 0pt
    - \fancyfoot[LO,LE]{Christian Marie <christian@ponies.io>}
    - \fancyfoot[RO,RE]{ https://github.com/anchor/roundtrip-aeson/ \linebreak
                         https://hackage.haskell.org/package/roundtrip/ \linebreak
		 	 http://www.informatik.uni-marburg.de/~rendel/unparse/
                       }
abstract: For everyone's sake, please interrupt for clarification on any of
          these concepts during the talk (after they are introduced).

---

\thispagestyle{fancy}
\pagenumbering{gobble}

1. Printer. [^1]

```haskell
type Printer a = a -> Doc
```

2. Parser. [^2]

```haskell
newtype Parser a = Parser (String -> [(a, String)])

```

3. Partial isomorphisms[^3].

```haskell
data Iso a b = Iso
    { apply   :: a -> Maybe b
    , unapply :: b -> Maybe a
    }
```

4. IsoFunctor: the functor[^4] from Iso to Hask (restricted to f).

```haskell
class IsoFunctor f where
  (<$>) :: Iso a b -> f a -> f b
```
5. ProductFunctor: a way to merge the output/input of two f's.

```haskell
class ProductFunctor f where
  -- Left associative, applies before <$>
  infixr 6 <*>
  (<*>) :: f a -> f b -> f (a, b)
```

6. Alternative: try one failing that, the other.

```haskell
class Alternative where
  (<|>) :: f a -> f a -> f a
```

7. Syntax: putting it all together.

```haskell
class (IsoFunctor s, ProductFunctor s, Alternative s) => Syntax s where
  -- (<$>) :: Iso a b -> f a -> f b
  -- (<*>) :: f a -> f b -> f (a, b)
  -- (<|>) :: f a -> f a -> f a
  pure :: Eq a => a -> s a -- Eq for checking the value at runtime

class Syntax s => JsonSyntax s where
    -- Nest the first syntax within the second.
    runSub :: s v -> s Value -> s v

    -- We need a concrete way to access the underlying Aeson Value types in
    -- order to work with them.
    value :: s Value
```

\newpage

[^3]: Isomorphism: A pair of functions $f : A \to B$ and $g : B \to A$ that
are inverses such that:

      1. $f \circ g \equiv id_B$
      2. $g \circ f \equiv id_A$

[^4]: Functor: A principled way of taking a "thing" and "moving" it into a
      different context whilst preserving some notion of structure.
      
      The canonical Haskell example is the Functor typeclass. Given two types
      (\mbox{$\forall{a,b} \in Hask$}) that have a function between them
      (\mbox{$\exists{f: a \to b}$}) we can produce a functored function
      (\mbox{$\forall F \in Functor.\ {F(f) : F(a) \to  F(b)}$}).

      More formally: a functor, F, is a transformation between categories
      $\mathcal{C}$ and $\mathcal{D}$ that maps morphisms and objects in
      $\mathcal{C}$ to morphisms and objects in $\mathcal{D}$ such that a few
      rules hold given objects $A, B \in \mathcal{C}$:
      
      1. $F(f : A \to B) = F(f) : F(A) \to F(B)$
      2. $F(id_A) = id_{F(A)}$
      3. $F(f \circ g) = F(f) \circ F(g)$

[^1]: Doc is an abstract document representation.

[^2]: The list here allows non-determinism (backtracking). The string in the
      result is the unparsed remainder.
