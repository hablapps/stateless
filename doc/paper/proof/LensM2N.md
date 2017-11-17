# Hypothesis: natural lens algebra is natural lens

If we provide a natural lens algebra for `State s`, `State a` with focus on `a`
we are actually instantiating a natural lens.

## From laws

`γ: (MonadState a q, Monad p) => q x -> p x` such that γ is a monad morphism:

`φ: State a ~> State s` such that φ is a monad morphism:
* MonMor0: `γ (return_a x) = return_s x`
* MonMor1: `γ (m >>=_a (x -> k x)) = γ m >>=_s (x -> γ (k x))`

Useful laws - *MonadState*:
* GetGet: `get >>= (a1 -> get >>= (a2 -> k (a1, a2)))`
* GetPut: `get >>= put = return ()`
* PutGet: `put a1 >> get = put a >> return a`
* PutPut: `put a1 >> put a2 = put a2`

## With defs

```haskell
φ :: State a ~> State s
φ = γ
```

## Proofs

#### MonMor0

```haskell
  φ (return_a x) => return_s x
= [def φ]
  γ (return_a x)
= [γ - MonMor0]
  return_s x
```

#### MonMor1

```haskell
  φ (m >>=_a (x -> k x)) => φ m >>=_s (x -> φ (k x))
= [def φ]
  γ (m >>=_a (x -> k x))
= [γ - MonMor1]
  γ m >>=_s (x -> γ (k x))
= [def φ]
  φ m >>=_s (x -> φ (k x))
```
