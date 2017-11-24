# Hypothesis: natural affine is an affine

## From laws

`φ: State a ~> MaybeT (State s)` such that φ holds:
* AffMor1: `φ (m >>=_a (x -> k x)) = φ m >>=_s (x -> φ (k x))`
* AffMor2: `φ q >> φ (return_a x) = φ q >> return_op x`
* AffMor3: `(uncurry (\out _ -> out) (φ q1 s)) $> a = (uncurry (\out _ -> out) (φ q2 s)) $> a`

Useful laws - *MonadState*:
* GetGet: `get >>= (a1 -> get >>= (a2 -> k (a1, a2))) = get >>= (a -> k (a, a))`
* GetPut: `get >>= put = return ()`
* PutGet: `put a >> get = put a >> return a`
* PutPut: `put a1 >> put a2 = put a2`

## With defs

```haskell
GETOPT :: s -> Maybe a
GETOPT s = uncurry (\out _ -> out) (φ get_a s)

PUT :: s -> a -> s
PUT s a = uncurry (\_ s -> s) (φ (put_a a) s)
```

## Proofs

#### GetPut

```haskell
  maybe s (PUT s) (GETOPT s) = s
= [lambda syntax]
  maybe s (\a -> PUT s a) (GETOPT s)
= [def PUT]
  maybe s (\a -> uncurry (\_ s -> s) (φ (put_a a) s)) (GETOPT s)
= [def GETOPT]
  maybe s (\a -> uncurry (\_ s -> s) (φ (put_a a) s)) (uncurry (\out _ -> out) (φ get_a s))


= [???]
  s
```

#### PutGet

```haskell
  GETOPT (PUT s a) = GETOPT s $> a
= [def PUT]
  GETOPT (uncurry (\_ s -> s) (φ (put_a a) s))
= [def GETOPT]
  uncurry (\out _ -> out) (φ get_a (uncurry (\_ s -> s) (φ (put_a a) s)))
= [Uncurry distribution]
  uncurry (\out _ -> out) (uncurry (\_ s -> φ get_a s) (φ (put_a a) s))
= [def apply/abstract]
  uncurry (\out _ -> out) ((s -> uncurry (\_ s -> φ get_a s) (φ (put_a a) s)) s)
= [def >>]
  uncurry (\out _ -> out) ((φ (put_a a) >> φ (get_a)) s)
= [AffMor1]
  uncurry (\out _ -> out) (φ (put_a a >> get_a) s)
= [MonadState - PutGet]
  uncurry (\out _ -> out) (φ (put_a a >> return_a a) s)
= [AffMor1]
  uncurry (\out _ -> out) ((φ (put_a a) >> φ (return_a a)) s)
= [AffMor2 ]
  uncurry (\out _ -> out) ((φ (put_a a) >> return_op a) s)
= [Monad - fmap]
  uncurry (\out _ -> out) ((φ (put_a a) $> a) s)
= [Uncurry distribution]
  (uncurry (\out _ -> out) (φ (put_a a) s)) $> a
= [AffMor3]
  (uncurry (\out _ -> out) (φ get_a s)) $> a
= [def GETOPT]
  GETOPT s $> a
```

#### PutPut

```haskell
  PUT (PUT s a1) a2 = PUT s a2
= [def PUT]
  PUT (uncurry (\_ s -> s) (φ (put_a a1) s)) a2
= [def PUT]
  uncurry (\_ s -> s) (φ (put_a a2) (uncurry (\_ s -> s) (φ (put_a a1) s)))
= [Uncurry distribution]
  uncurry (\_ s -> s) (uncurry (\_ s -> φ (put_a a2) s) (φ (put_a a1) s))
= [def apply/abstract]
  uncurry (\_ s -> s) ((s -> uncurry (\_ s -> φ (put_a a2) s) (φ (put_a a1) s)) s)
= [def >>]
  uncurry (\_ s -> s) ((φ (put_a a1) >> φ (put_a a2)) s)
= [φ distributes over >>]
  uncurry (\_ s -> s) (φ (put_a a1 >> put_a a2) s)
= [MonadState - PutPut]
  uncurry (\_ s -> s) (φ (put_a a2) s)
= [def PUT]
  PUT s a2
```
