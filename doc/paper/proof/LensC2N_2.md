# Hypothesis: lens is a natural lens

## From laws

`Lens { get :: s -> a; put :: s -> a -> s }` such that:
* GetPut: `put s (get s) = s`
* PutGet: `get (put s a) = a`
* PutPut: `put (put s a1) a2 => put s a2`

## With defs

```haskell
φ :: forall x . State a x ~> State s x
φ f = s -> second (put s) (f (get s))
```

## With utils

State definition:
```haskell
type State s x = s -> (x, s)
```

Bind definition:
```haskell
(>>=) :: State s x -> (x -> State s y) -> State s y
sx >>= f = s -> uncurry (\x s2 -> f x s2) (sx s)
```

Useful law shortcuts:
```haskell
uncurry g . second f  = uncurry (\x s -> g x (f s)) -- Uncurry 2
uncurry (f . g) = f . uncurry g                     -- Distribution
```

## Proofs

#### MonMor0

```haskell
  φ (return_a x) => return_s x
= [def φ]
  s -> second (put s) (return_a x (get s))
= [def return_a]
  s -> second (put s) ((a -> (x, a)) (get s))
= [def $]
  s -> second (put s) (x, get s)
= [def second]
  s -> (x, put s (get s))
= [GetPut]
  s -> (x, s)
= [def return_s]
  return_s x
```

#### MonMor1

```haskell
  φ m >>=_s (x -> φ (k x)) => φ (m >>=_a (x -> k x))
= [def φ]
  (s -> second (put s) (m (get s))) >>=_s (x -> s -> second (put s) ((k x) (get s)))
= [def >>=_s]
  s -> uncurry (\x s2 -> ((x -> s -> second (put s) ((k x) (get s)))) x s2) ((s -> second (put s) (m (get s))) s)
= [def $]
  s -> uncurry (\x s2 -> second (put s2) ((k x) (get s2))) (second (put s) (m (get s)))
= [def .]
  s -> (uncurry (\x s2 -> second (put s2) ((k x) (get s2))) . second (put s)) (m (get s))
= [Uncurry 2]
  s -> uncurry (\x a2 -> (\x s2 -> second (put s2) ((k x) (get s2))) x (put s a2)) (m (get s))
= [def $]
  s -> uncurry (\x a2 -> second (put (put s a2)) ((k x) (get (put s a2)))) (m (get s))
= [PutPut]
  s -> uncurry (\x a2 -> second (put s) ((k x) (get (put s a2)))) (m (get s))
= [PutGet]
  s -> uncurry (\x a2 -> second (put s) ((k x) a2)) (m (get s))
= [Distribution]
  s -> (second (put s) . uncurry (\x a2 -> (k x) a2)) (m (get s))
= [def .]
  s -> second (put s) (uncurry (\x a2 -> (k x) a2) (m (get s)))
= [def apply/abstract]
  s -> second (put s) ((a -> uncurry (\x a2 -> (k x) a2) (m a)) (get s))
= [def >>=_a]
  s -> second (put s) ((m >>= (x -> k x)) (get s))
= [def φ]
  φ (m >>=_a (x -> k x))
```
