# Hypothesis: affine algebra state instance is an affine

If we provide an affine algebra for `State s` with focus on `a` we are actually
instantiating an affine.

## From laws

`AffineAlg { getOpt :: State s (Maybe a), putOpt :: a -> State s (Maybe ()) }` such that:
* GetGet: `getOpt >>= (a1 -> getOpt >>= (a2 -> k (a1, a2))) = getOpt >>= (a -> k (a, a))`
* GetPut: `getOpt >>= (maybe (return None) putOpt) = fmap (() <$) getOpt`
* PutGet: `putOpt a >> getOpt = fmap (a <$) (putOpt a)`
* PutPut: `putOpt a1 >> putOpt a2 = putOpt a2`
* Homog: `getOpt >>= (oa -> putOpt x >> return (() <$ oa)) = putOpt x`
* Noneff: `getOpt >> p = p` (not inferred from GetGet)

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
uncurry (f . g) = f . uncurry g -- Uncurry distribution
uncurry (\out _ -> out) (uncurry (\out s -> (f out, g s)) s) = uncurry (\out _ -> f out) s -- Uncurry out
```

## Proofs

#### GetPut

```haskell
  maybe s (\a -> PUT s a) (GETOPT s) => s
= [def PUT]
  maybe s (\a -> uncurry (\_ s -> s) (putOpt a s)) (GETOPT s)
= [def GETOPT]
  maybe s (\a -> uncurry (\_ s -> s) (putOpt a s)) (uncurry (\out _ -> out) (getOpt s))
= [Non-effectufl GETOPT: NonEff + GetGet]
  -- This step seems like a big jump, but it's trivial using diagramas. We're
  -- just using `getOpt` state output to feed the rest of the program, instead
  -- of using `s` directly.
  uncurry (\out s -> maybe s (\a -> uncurry (\_ s -> s) (putOpt a s)) out) (getOpt s)
= [def uncurry, def return]
  uncurry (\out s -> maybe (uncurry (\_ s -> s) (return None s)) (\a -> uncurry (\_ s -> s) (putOpt a s)) out) (getOpt s)
= [lift uncurry]
  uncurry (\out s -> uncurry (\_ s -> s) (maybe (return None s) (\a -> putOpt a s) out)) (getOpt s)
= [Uncurry distribution]
  uncurry (\_ s -> s) (uncurry (\out s -> maybe (return None s) (\a -> putOpt a s) out) (getOpt s))
= [def apply/abstract]
  uncurry (\_ s -> s) ((s -> uncurry (\out s -> maybe (return None s) (\a -> putOpt a s) out) (getOpt s)) s)
= [def >>=]
  uncurry (\_ s -> s) ((getOpt >>= (maybe (return None) putOpt)) s)
= [def GetPut]
  uncurry (\_ s -> s) ((fmap ($> ()) getOpt) s)
= [def fmap]
  uncurry (\_ s -> s) (uncurry (\out s -> (out $> (), s)) (getOpt s))
= [Uncurry state]
  uncurry (\_ s -> s) (getOpt s)
= [Noneff]
  s
```

#### PutGet

```haskell
  GETOPT (PUT s a) => GETOPT s $> a
= [def GETOPT]
  uncurry (\out _ -> out) (getOpt (PUT a s))
= [def PUT]
  uncurry (\out _ -> out) (getOpt (uncurry (\_ s -> s) (putOpt a s)))
= [Uncurry distribution]
  uncurry (\out _ -> out) (uncurry (\_ s -> getOpt s) (putOpt a s))
= [def apply/abstract]
  uncurry (\out _ -> out) ((s -> uncurry (\_ s -> getOpt s) (putOpt a s)) s)
= [def >>]
  uncurry (\out _ -> out) ((putOpt a >> getOpt) s)
= [PutGet]
  uncurry (\out _ -> out) ((fmap (a <$) (putOpt a)) s)
= [Homog]
  uncurry (\out _ -> out) ((fmap (a <$) (getOpt >>= (oa -> putOpt a >> return (() <$ oa)))) s)
= [def fmap]
  uncurry (\out _ -> out) ((getOpt >>= (oa -> putOpt a >> return (a <$ oa))) s)
= [def >>]
  uncurry (\out _ -> out) ((getOpt >>= (oa -> (s -> uncurry (\_ s -> return (a <$ oa) s) (putOpt a s)))) s)
= [def return]
  uncurry (\out _ -> out) ((getOpt >>= (oa -> (s -> uncurry (\_ s -> ((a <$ oa), s)) (putOpt a s)))) s)
= [def >>=]
  uncurry (\out _ -> out) ((s -> uncurry (\out s -> (oa -> (s -> uncurry (\_ s -> ((a <$ oa), s)) (putOpt a s))) out s) (getOpt s)) s)
= [def apply]
  uncurry (\out _ -> out) ((s -> uncurry (\out s -> uncurry (\_ s -> ((a <$ out), s)) (putOpt a s)) (getOpt s)) s)
= [def apply]
  uncurry (\out _ -> out) (uncurry (\out s -> uncurry (\_ s -> ((a <$ out), s)) (putOpt a s)) (getOpt s))
= [Uncurry distribution]
  uncurry (\out _ -> out) (uncurry (\out s -> (a <$ out, uncurry (\_ s -> s) (putOpt a s))) (getOpt s))
= [Uncurry out: f = (a <$); g = uncurry (\_ s -> s) . putOpt a]
  uncurry (\out _ -> a <$ out) (getOpt s)
= [Uncurry distribution]
  a <$ (uncurry (\out _ -> out) (getOpt s))
= [def GETOPT]
  a <$ GETOPT s
= [flip <$]
  GETOPT s $> a
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
