# Finding affine algebras

This file tries to determine the structure and laws for affine algebras. First
of all, we simply find the buttons (input, output and state transformation).
This definitions will be in terms of `uncurry`. Then we'll try to represent them
in terms of monadic combinators. Since this is not always possible, we'll need
to create laws in terms of monads, which are, generally, more constrained than
the ones corresponding to affine.

## From

Operations:
* `GETOPT :: s -> Maybe a`
* `PUT :: s -> a -> s`

With laws:
* PutGet: `GETOPT (PUT s a) => GETOPT s $> a`
* GetPut: `maybe (PUT s) (GETOPT s) => s`
* PutPut: `PUT (PUT s a1) a2 => PUT s a2`

## Finding the buttons

```haskell
  (GETOPT :: s -> Maybe a, PUT :: s -> a -> s)
  { GETOPT (PUT s a) => GETOPT s $> a
  , maybe (PUT s) (GETOPT s) => s
  , PUT (PUT s a1) a2 => PUT s a2 }

= [getOpt + input]
  (GETOPT :: () -> s -> Maybe a, PUT :: s -> a -> s)
  { GETOPT () (PUT s a) => GETOPT () s $> a
  , maybe (PUT s) (GETOPT () s) => s
  , PUT (PUT s a1) a2 => PUT s a2 }

= [getOpt + resulting state]
  (GETOPT :: () -> s (Maybe a, s), PUT :: s -> a -> s)
  { uncurry (\out _ -> out) (GETOPT () (PUT s a)) => uncurry (\out _ -> out $> a) (GETOPT () s)
  , uncurry (\out _ -> maybe s (PUT s) out) (GETOPT () s) => s
  , PUT (PUT s a1) a2 => PUT s a2 }

= [put + def flip]
  (GETOPT :: () -> s (Maybe a, s), PUT :: a -> s -> s)
  { uncurry (\out _ -> out) (GETOPT () (PUT a s)) => uncurry (\out _ -> out $> a) (GETOPT () s)
  , uncurry (\out _ -> maybe s (flip (PUT s)) out) (GETOPT () s) => s
  , PUT a2 (PUT a1 s) => PUT a2 s }

= [put + optional output]
  (GETOPT :: () -> s (Maybe a, s), PUT :: a -> s -> (Maybe (), s)
  { uncurry (\out _ -> out) (GETOPT () (uncurry (\_ s -> s) (PUT a s))) => uncurry (\out _ -> out $> a) (GETOPT () s)
  , uncurry (\out _ -> maybe s (\a -> uncurry (\_ s -> s) (PUT a s)) out) (GETOPT () s) => s
  , uncurry (\_ s -> s) (PUT a2 (uncurry (\_ s -> s) (PUT a1 s))) => uncurry (\_ -> s) (PUT a2 s) }

= [def State]
  (GETOPT :: () -> State s (Maybe a), PUT :: a -> State s (Maybe ()))
  { uncurry (\out _ -> out) (GETOPT () (uncurry (\_ s -> s) (PUT a s))) => uncurry (\out _ -> out $> a) (GETOPT () s)
  , uncurry (\out _ -> maybe s (\a -> uncurry (\_ s -> s) (PUT a s)) out) (GETOPT () s) => s
  , uncurry (\_ s -> s) (PUT a2 (uncurry (\_ s -> s) (PUT a1 s))) => uncurry (\_ -> s) (PUT a2 s) }
```

## Abstracting

Using definitions:
```haskell
fmap :: (x -> y) -> State s x -> State s y
fmap f sx = s -> uncurry (\out s -> (f out, s)) (sx s)

(>>) :: State s x -> State s y -> State s y
sx >> sy = s -> uncurry (\_ s -> sy s) (sx s)

(>>=) :: State s x -> (x -> State s y) -> State s y
sx >>= f = s -> uncurry (\x s2 -> f x s2) (sx s)
```

And laws:
```haskell
uncurry g . second f  = uncurry (\x s -> g x (f s)) -- Uncurry 2
uncurry (f . g) = f . uncurry g                     -- Uncurry distribution
```

#### New laws

What should we do with the contrived information we added in the previous
section? We need some basic rules for it:
* Get is non-effectful: `uncurry (\_ s -> s) (GETOPT () s) = s`
* Outputs are homogeneous for a state: `uncurry (\out _ -> out) (PUT a s) = uncurry (\out _ -> out $> ()) (GETOPT () s)`

Temptative laws (taking previous laws to monadic context):
* Get is non-effectful: `getOpt >> p = p`
* Outputs are homogeneous: `getOpt >>= (oa -> putOpt x >> return (() <$ oa)) = putOpt x`

#### 1st law

```haskell
  uncurry (\out _ -> out) (GETOPT () (uncurry (\_ s -> s) (PUT a s))) => uncurry (\out _ -> out $> a) (GETOPT () s)
= [Uncurry distribution]
  uncurry (\out _ -> out) (uncurry (\_ s ->  GETOPT () s) (PUT a s)) => uncurry (\out _ -> out $> a) (GETOPT () s)
= [def apply/abstract]
  uncurry (\out _ -> out) ((s -> uncurry (\_ s ->  GETOPT () s) (PUT a s)) s) => uncurry (\out _ -> out $> a) (GETOPT () s)
= [def >>]
  uncurry (\out _ -> out) ((PUT a >> GETOPT ()) s) => uncurry (\out _ -> out $> a) (GETOPT () s)
```

What about state? Are we interested in holding them for this scenario? Is there
a connection with the rest of new laws?

Temptative law:
`putOpt a >> getOpt = fmap (a <$) (putOpt a)`

This is in fact a consequence of the new laws we added before.

#### 2nd law

```haskell
  uncurry (\out _ -> maybe s (\a -> uncurry (\_ s -> s) (PUT a s)) out) (GETOPT () s) => s
= [Non-effectful get (new law)]
  uncurry (\out s -> maybe s (\a -> uncurry (\_ s -> s) (PUT a s)) out) (GETOPT () s) => s
= [def apply/abstract]
  (s -> uncurry (\out s -> maybe s (\a -> uncurry (\_ s -> s) (PUT a s)) out) (GETOPT () s)) s => s
```

Not a `>>=`, since `f` isn't returning an output. We could establish some
constraints for this situation, in order to return a `Nothing` when focus isn't
available and `Just` otherwise.

Temptative law:
`getOpt >>= (maybe (return None) setOpt) = fmap (() <$) getOpt`

This is adding information about the output. If there's no focus, then we return
None. Otherwise, we return `setOpt`. This is claiming that the focus should be
preserved by setOpt.

#### 3rd law

```haskell
  uncurry (\_ s -> s) (PUT a2 (uncurry (\_ s -> s) (PUT a1 s))) => uncurry (\_ -> s) (PUT a2 s)
= [Uncurry distribution]
  uncurry (\_ s -> s) (uncurry (\_ s -> PUT a2 s) (PUT a1 s)) => uncurry (\_ -> s) (PUT a2 s)
= [def apply/abstract]
  uncurry (\_ s -> s) ((s -> uncurry (\_ s -> PUT a2 s) (PUT a1 s)) s) => uncurry (\_ -> s) (PUT a2 s)
= [def >>]
  uncurry (\_ s -> s) ((PUT a1 >> PUT a2) s) => uncurry (\_ -> s) (PUT a2 s)
```

What about the outputs? Are we interested in holding them for this scenario? Is
there a connection with the rest of new laws?

Temptative law:
`putOpt a1 >> putOpt a2 = putOpt a2`

This temptative law is more restrictive than the previous one, since we're not
only restricting the state values, but also the outputs. This is ok, since
outputs should be the same for both cases.

## Affine Algebra Definition and Laws

Affine algebra:
```haskell
AffineAlg { getOpt :: p (Maybe a)
          , setOpt :: a -> p () }
```

With laws:
* Non-eff: `getOpt >> p = p`
* Homog: `getOpt >>= (oa -> putOpt x >> return (() <$ oa)) = putOpt x`
* PutGet: `putOpt a >> getOpt = fmap (a <$) (putOpt a)`
* GetPut: `getOpt >>= (maybe (return None) setOpt) = fmap (() <$) getOpt`
* PutPut: `putOpt a1 >> putOpt a2 = putOpt a2`
