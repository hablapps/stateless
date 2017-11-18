# Hypothesis: algebraic natural lens is an algebraic lens

## From laws

`γ: (MonadState a q, Monad p) => q x -> p x` such that γ is a mona morphism:
* MonMor0: `γ (return_a x) = return_s x`
* MonMor1: `γ (m >>=_a (x -> k x)) = γ m >>=_s (x -> γ (k x))`

Useful laws - *MonadState*:
* GetGet: `get >>= (a1 -> get >>= (a2 -> k (a1, a2))) = get >>= (a -> k (a, a))`
* GetPut: `get >>= put = return ()`
* PutGet: `put a >> get = put a >> return a`
* PutPut: `put a1 >> put a2 = put a2`

## With defs

```haskell
get_p :: p a
get_p = γ get

put_p :: a -> p ()
put_p = γ (put a)
```

## Proofs

#### GetGet

```haskell
  get_p >>=_p (a1 -> get_p >>=_p (a2 -> k (a1, a2))) => get_p >>=_p (a -> k (a, a))
= [def get_p]
  γ get >>=_p (a1 -> γ get >>=_p (a2 -> k (a1, a2)))
= [do-notation]
  do_p { a1 <- γ get; do_p { a2 <- γ get; k (a1, a2) } }
= [Monad associativity]
  do_p { (a1, a2) <- do_p { a1 <- γ get; a2 <- γ get; return (a1, a2) } k (a1, a2) }
= [do-notation] -- reasoning about associativity is easier with do-notation
  (γ get >>=_p (a1 -> γ get >>=_p (a2 -> return (a1, a2)))) >>=_p ((a1, a2) -> k (a1, a2))
= [γ - MonMor0]
  (γ get >>=_p (a1 -> γ get >>=_p (a2 -> γ (return (a1, a2))))) >>=_p ((a1, a2) -> k (a1, a2))
= [γ - MonMor1]
  (γ (get >>=_p (a1 -> get >>=_p (a2 -> return (a1, a2))))) >>=_p ((a1, a2) -> k (a1, a2))
= [MonadState - GetGet]
  (γ (get >>=_q (a -> return (a, a)))) >>=_p ((a1, a2) -> k (a1, a2))
= [γ - MonMor1]
  (γ get >>=_p (a -> γ (return (a, a)))) >>=_p ((a1, a2) -> k (a1, a2))
= [γ - MonMor0]
  (γ get >>=_p (a -> return (a, a))) >>=_p ((a1, a2) -> k (a1, a2))
= [do-notation]
  do_p { (a1, a2) <- do_p { a <- γ get; return (a, a) }; k (a1, a2) }
= [Monad associativity]
  do_p { a <- γ get; (a1, a2) <- return (a, a); k (a1, a2) }
= [do-notation]
  γ get >>=_p (a -> return (a, a) >>=_p ((a1, a2) -> k (a1, a2)))
= [lambda syntax]
  γ get >>=_p (a -> return (a, a) >>=_p k)
= [Monad - left identity]
  γ get >>=_p (a -> k (a, a))
= [def get]
  get_p >>=_p (a -> k (a, a))
```

#### GetPut

```haskell
  get_p >>=_p put_p = return_p ()
= [def get_p]
  γ get >>=_p put_p
= [lambda syntax]
  γ get >>=_p (x -> put_p x)
= [def put_p]
  γ get >>=_p (x -> γ (put x))
= [γ - MonMor1]
  γ (get >>=_q (x -> put x))
= [lambda syntax]
  γ (get >>=_q put)
= [MonadState - GetPut]
  γ (return ())
= [γ - MonMor0]
  return_p ()
```

#### PutGet

```haskell
  put_p a1 >>_p get_p => put_p a >>_p return_p a
= [def get_p]
  put_p a1 >>_p γ get
= [def put_p]
  γ (put a) >>_p γ get
= [γ - MonMor1]
  γ (put a >>_q get)
= [MonadState - PutGet]
  γ (put a >>_q return a)
= [γ - MonMor1]
  γ (put a) >>_p γ (return a)
= [γ - MonMor1]
  γ (put a) >>_p return_p a
= [def put_p]
  put_p a >>_p return_p a
```

#### PutPut

```haskell
  put_p a1 >>_p put_p a2 => put_p a2
= [def put_p]
  γ (put a1) >>_p γ (put a2)
= [γ - MonMor1]
  γ (put a1 >>_q put a2)
= [MonadState - PutPut]
  γ (put a2)
= [def put_p]
  put_p a2
```
