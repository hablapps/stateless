# Hypothesis: natural lens is a lens

## From laws

`φ: State a ~> State s` such that φ is a monad morphism:
* MonMor0: `φ (return_a x) = return_s x`
* MonMor1: `φ (m >>=_a (x -> k x)) = φ m >>=_s (x -> φ (k x))`

Useful laws - *MonadState*:
* GetGet: `get >>= (a1 -> get >>= (a2 -> k (a1, a2)))`
* GetPut: `get >>= put = return ()`
* PutGet: `put a1 >> get = put a >> return a`
* PutPut: `put a1 >> put a2 = put a2`

## With defs

```haskell
get :: s -> a
get s = eval (φ get_a) s

put :: a -> s -> s
put a s = exec (φ (put_a a)) s
```

## Proofs

#### GetPut

```haskell
  put (get s) s => s
= [def get]
  put (eval (φ get_a) s) s
= [def put]
  exec (φ (put_a (eval (φ get_a) s))) s
= [def >>=, MonMor1 (φ get (φ (put x) == φ get >> φ (put x)))]
  exec (φ get_a >>= (x -> φ (put_a x))) s
= [MonMor1]
  exec (φ (get_a >>= (x -> put_a x))) s
= [MonadState - GetPut]
  exec (φ (return ())) s
= [MonMor0]
  exec (return ()) s
= [def return, def exec]
  s
```

#### PutGet

```haskell
  get (put a s) => a
= [def put]
  get (exec (φ (put_a a)) s)
= [def get]
  eval (φ get_a) (exec (φ (put_a a)) s)
= [def >>]
  eval (φ (put_a a) >>_s φ get_a) s
= [MonMor1]
  eval (φ (put_a a >>_a get_a)) s
= [MonadState - PutGet]
  eval (φ (put_a a >>_a return_a a)) s
= [MonMor1]
  eval (φ (put_a a) >>_s φ (return_a a)) s
= [MonMor0]
  eval (φ (put_a a) >>_s return_s a) s
= [def >>]
  eval (return_s a) (exec (φ (put_a a)) s)
= [def eval, def return] -- big jump, but safe! :)
  a
```

#### PutPut

```haskell
  put a2 (put a1 s) => put a2 s
= [def put]
  exec (φ (put_a a2)) (exec (φ (put_a a1)) s)
= [def >>]
  exec (φ (put_a a1) >>_s φ (put_a a2)) s
= [MonMor1]
  exec (φ (put_a a1 >>_a put_a a2)) s
= [MonadState - PutPut]
  exec (φ (put_a a2)) s
= [def put]
  put a2 s
```
