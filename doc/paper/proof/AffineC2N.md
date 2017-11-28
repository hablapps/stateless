# Hypothesis: affine is a natural affine

## From laws

`Affine { getOpt :: s -> Maybe a; put :: a -> s -> s }` such that:
* GetPut: `maybe s (put s) (getOpt s) = s`
* PutGet: `getOpt (put a s) = (getOpt s) $> a`
* PutPut: `put a2 (put a1 s) => put a2 s`

## With defs

```haskell
φ :: State a ~> MaybeT (State s)
φ sa = s -> maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (sa a)) (getOpt s)
```

## Proofs

#### AffMor1
```haskell
  φ (m >>=_a (x -> k x)) = φ m >>=_s (x -> φ (k x))
```

#### AffMor2
```haskell
  φ q >> φ (return_a x) = φ q >> return_op x
= [def φ]
  (s -> maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (q a)) (getOpt s)) >> φ (return_a x)
= [def φ]
  (s -> maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (q a)) (getOpt s)) >>
    (s -> maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (return_a x a)) (getOpt s))
```

#### AffMor3
```haskell
  (uncurry (\out _ -> out) (φ q1 s)) $> x = (uncurry (\out _ -> out) (φ q2 s)) $> x
= [def φ]
  (uncurry (\out _ -> out) (maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (q1 a)) (getOpt s))) $> x
= [Uncurry distribution]
  (maybe (uncurry (\out _ -> out) (None, s)) (\a -> uncurry (\out _ -> out) (uncurry (\out a -> (Just out, put s a)) (q1 a))) (getOpt s)) $> x
= [def uncurry]
  (maybe None (\a -> uncurry (\out _ -> out) (uncurry (\out a -> (Just out, put s a)) (q1 a))) (getOpt s)) $> x
= [Uncurry out]
  (maybe None (\a -> uncurry (\out _ -> Just out) (q1 a)) (getOpt s)) $> x
= [maybe distribution]
  maybe (None $> x) (\a -> uncurry (\out _ -> Just out $> x) (q1 a)) (getOpt s)
= [def $>]
  maybe (None $> x) (\a -> uncurry (\out _ -> Just () $> x) (q1 a)) (getOpt s)
= [ignores (q1 a)]
  maybe (None $> x) (\a -> Just () $> x) (getOpt s)
  -- now, undo everything!
= [ignores (q2 a)]
  maybe (None $> x) (\a -> uncurry (\out _ -> Just () $> x) (q2 a)) (getOpt s)
= [def $>]
  maybe (None $> x) (\a -> uncurry (\out _ -> Just out $> x) (q2 a)) (getOpt s)
= [maybe distribution]
  (maybe None (\a -> uncurry (\out _ -> Just out) (q2 a)) (getOpt s)) $> x
= [Uncurry out]
  (maybe None (\a -> uncurry (\out _ -> out) (uncurry (\out a -> (Just out, put s a)) (q2 a))) (getOpt s)) $> x
= [def uncurry]
  (maybe (uncurry (\out _ -> out) (None, s)) (\a -> uncurry (\out _ -> out) (uncurry (\out a -> (Just out, put s a)) (q2 a))) (getOpt s)) $> x
= [Uncurry distribution]
  (uncurry (\out _ -> out) (maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (q2 a)) (getOpt s))) $> x
= [def φ]
  (uncurry (\out _ -> out) (φ q2 s)) $> x
```

#### AffMor4 (return)
```haskell
  f (uncurry (\out _ -> out) (φ (return_a x) s)) s = uncurry f (φ (return_a x) s)
= [def φ]
  f (uncurry (\out _ -> out) (maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (return_a x a)) (getOpt s))) s
= [def return]
  f (uncurry (\out _ -> out) (maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (x, a)) (getOpt s))) s
= [def uncurry]
  f (uncurry (\out _ -> out) (maybe (None, s) (\a -> (Just x, put s a)) (getOpt s))) s
= [split maybe]
  f (uncurry (\out _ -> out) (maybe None (\_ -> Just x) (getOpt s), maybe s (put s) (getOpt s))) s
= [GetPut]
  f (uncurry (\out _ -> out) (maybe None (\_ -> Just x) (getOpt s), s)) s
= [Uncurry distribution]
  uncurry (\out _ -> f out s) (maybe None (\_ -> Just x) (getOpt s), s)
= [s == s]
  uncurry (\out s -> f out s) (maybe None (\_ -> Just x) (getOpt s), s)
= [lambda syntax]
  uncurry f (maybe None (\_ -> Just x) (getOpt s), s)
= [GetPut]
  uncurry f (maybe None (\_ -> Just x) (getOpt s), maybe s (put s) (getOpt s))
= [unsplit maybe]
  uncurry f (maybe (None, s) (\a -> (Just x, put s a)) (getOpt s))
= [def uncurry]
  uncurry f (maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (x, a)) (getOpt s))
= [def return]
  uncurry f (maybe (None, s) (\a -> uncurry (\out a -> (Just out, put s a)) (return_a x a)) (getOpt s))
= [def φ]
  uncurry f (φ (return_a x) s)
```
