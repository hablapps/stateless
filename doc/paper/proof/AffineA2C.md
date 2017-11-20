# Hypothesis: affine algebra state instance is an affine

If we provide an affine algebra for `State s` with focus on `a` we are actually
instantiating an affine.

## From laws

`AffineAlg { getOpt :: State s (Maybe a), putOpt :: a -> State s (Maybe ()) }` such that:
* GetGet: `getOpt >>= (a1 -> getOpt >>= (a2 -> k (a1, a2))) = getOpt >>= (a -> k (a, a))`
* GetPut: `getOpt >>= (maybe (return None) setOpt) = getOpt >>= (() <$)`
* PutGet: `putOpt a >> getOpt = fmap (a <$) (putOpt a)`
* PutPut: `putOpt a1 >> putOpt a2 = putOpt a2`

## With defs

```haskell
GETOPT s = uncurry (\ox _ -> ox) (getOpt s)
PUT s a = uncurry (\_ s -> s) (putOpt a s)
```

## With utils

```haskell
type State s x = s -> (x, s)
```

Bind definition:
```haskell
(>>=) :: State s x -> (x -> State s y) -> State s y
sx >>= f = s -> uncurry (\x s2 -> f x s2) (sx s)

(>>) State s x -> State s y -> State s y
sx >> sy = s -> uncurry (\_ s -> sy s) (sx s)
```

Useful law shortcuts:
```haskell
uncurry g . second f  = uncurry (\x s -> g x (f s)) -- Uncurry 2
uncurry (f . g) = f . uncurry g                     -- Uncurry distribution
```

## Proofs

#### GetPut

#### PutGet

```haskell
  GETOPT (PUT a s) => GETOPT s $> a
= [def GETOPT]
```

#### PutPut

```haskell
  PUT (PUT s a1) a2 => PUT s a2
= [def put]
  uncurry (\_ s -> s) (putOpt a2 (uncurry (\_ s -> s) (putOpt a1 s)))
= [def .]
  uncurry (\_ s -> s) ((putOpt a2 . uncurry (\_ s -> s)) (putOpt a1 s))
= [Uncurry distribution]
  uncurry (\_ s -> s) (uncurry ((putOpt a2) . (\_ s -> s)) (putOpt a1 s))
= [lambda syntax]
  uncurry (\_ s -> s) (uncurry (\ox s -> ((putOpt a2) . (\_ s -> s)) ox s) (putOpt a1 s))
= [def .]
  uncurry (\_ s -> s) (uncurry (\ox s -> (putOpt a2 ((\_ s -> s) ox s))) (putOpt a1 s))
= [def apply/abstract]
  uncurry (\_ s -> s) (uncurry (\ox s -> putOpt a2 s) (putOpt a1 s))
= [ignored parameter]
  uncurry (\_ s -> s) (uncurry (\_ s -> putOpt a2 s) (putOpt a1 s))
= [def apply/abstract]
  uncurry (\_ s -> s) ((s -> uncurry (\_ s -> putOpt a2 s) (putOpt a1 s)) s)
= [def >>]
  uncurry (\_ s -> s) ((putOpt a1 >> putOpt a2) s)
= [AffineAlg - PutPut]
  uncurry (\_ s -> s) (putOpt a2 s)
= [def PUT]
  PUT a2 s
```
