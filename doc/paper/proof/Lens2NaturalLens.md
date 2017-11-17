# Hypothesis: lens is a natural lens

## From laws

`Lens { get :: s -> a; set :: a -> s -> s }` such that:
* GetPut: `put (get s) s = s`
* PutGet: `get (put a s) = a`
* PutPut: `put a2 (put a1 s) => put a2 s`

## With defs

```haskell
φ :: State a ~> State s
φ sa = State (s -> fmap (flip put s) (run sa (get s)))
```

## Proofs

#### MonMor0

```haskell
  φ (return_a x) => return_s x
= [def φ]
  State (s -> fmap (flip put s) (run (return_a x) (get s)))
= [def return, def run]
  State (s -> fmap (flip put s) (x, get s))
= [def fmap]
  State (s -> (x, flip put s (get s)))
= [def flip]
  State (s -> (x, put (get s) s))
= [GetPut]
  State (s -> (x, s))
= [def return]
  return_s x
```

#### MonMor1

```haskell
  φ m >>=_s (x -> φ (k x)) => φ (m >>=_a (x -> k x))
= [def φ]
  State (s -> fmap (flip put s) (run m (get s))) >>=_s (x ->
    State (s -> fmap (flip put s) (run (k x) (get s))))
= [def >>=_s]
  State (s0 ->
    let (s1, x1) = fmap (flip put s0) (run m (get s0))
    fmap (flip put s1) (run (k x1) (get s1)))
= [def eval, def run]
  State (s0 ->
    let s1 = put (exec m (get s0)) s0
        x1 = eval m (get s0)
    fmap (flip put s1) (run (k x1) (get s1)))
= [s1 expansion]
  State(s0 ->
    let s1 = put (exec m (get s0)) s0
        x1 = eval m (get s0)
    fmap (flip put s1) (run (k x1) (get (put (exec m (get s0)) s0))))
= [PutGet]
  State(s0 ->
    let s1 = put (exec m (get s0)) s0
        x1 = eval m (get s0)
    fmap (flip put s1) (run (k x1) (exec m (get s0))))
= [s1 expansion]
  State(s0 ->
    let x1 = eval m (get s0)
    fmap (flip put (put (exec m (get s0)) s0)) (run (k x1) (exec m (get s0))))
= [PutPut]
  State(s0 ->
    let x1 = eval m (get s0)
    fmap (flip put s0) (run (k x1) (exec m (get s0))))
= [x1 expansion, s0 -> s]
  State(s -> fmap (flip put s) (run (k (eval m (get s))) (exec m (get s))))
= [def >>=]
  State(s -> fmap (flip put s) (run (m >>=_a k) (get s)))
= [def φ]
  φ (m >>=_a k)
= [lambda syntax]
  φ (m >>=_a (x -> k x))
```
