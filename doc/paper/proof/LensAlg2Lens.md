# Hypothesis: lens algebra state instance is a lens

If we provide a lens algebra for `State s` with focus on `a` we are actually
instantiating a lens.

## From laws

`LensAlg { get :: State s a; set :: a -> State s () }` such that:
* GetGet: `get >>= (a1 -> get >>= (a2 -> k (a1, a2)))`
* GetPut: `get >>= put = return ()`
* PutGet: `put a1 >> get = put a >> return a`
* PutPut: `put a1 >> put a2 = put a2`

## With defs

```haskell
GET s = eval get s
PUT s a = exec (put a) s
```

## Proofs

#### GetPut

```haskell
  PUT (GET s) s => s
= [def GET]
  PUT (eval get s) s
= [def PUT]
  exec (put (eval get s)) s
= [non-effectful return]
  exec (put (eval get s)) (exec (return ()) s)
= [Lens algebra - GetPut]
  exec (put (eval get s)) (exec (get >>= put) s)
= [Lens algebra - GetGet]
  exec (put (eval get s)) (exec (get >>= (_ -> get >>= put)) s)
= [Lens algebra - GetPut]
  exec (put (eval get s)) (exec (get >>= (_ -> return ())) s)
= [def >>]
  exec (put (eval get s)) (exec (get >> return ()) s)
= [non-effectful return]
  exec (put (eval get s)) (exec get s)
= [def >>=]
  exec (get >>= put) s
= [Lens algebra - GetPut]
  exec (return ()) s
= [def return, def exec]
  s
```

#### PutGet

```haskell
  GET (PUT a s) => a
= [def PUT]
  GET (exec (put a) s)
= [def GET]
  eval get (exec (put a) s)
= [def >>]
  eval (put a >> get) s
= [Lens algebra - PutGet]
  eval (put a >> return a) s
= [def >>]
  eval (return a) (put a s)
= [def eval, def return]
  a
```

#### PutPut

```haskell
  PUT a2 (PUT a1 s) => PUT a2 s
= [def PUT]
  exec (put a2) (exec (put a1) s)
= [def >>]
  exec (put a1 >> put a2) s
= [Lens algebra - PutPut]
  exec (put a2) s
= [def PUT]
  PUT a2 s
```
