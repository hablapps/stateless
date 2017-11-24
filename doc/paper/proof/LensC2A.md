# Finding lens algebras

## From

Operations:
* `GET :: s -> a`
* `PUT :: s -> a -> s`

With laws:
* GetPut: `PUT s (GET s) => s`
* PutGet: `GET (PUT s a) => a`
* PutPut: `PUT (PUT s a1) a2 => PUT s a2`

## Finding the buttons

```haskell
  (GET :: s -> a, PUT :: s -> a -> s)
  { PUT s (GET s) => s
  , GET (PUT s a) => a
  , PUT (PUT s a1) a2 => PUT s a2 }

= [GET + input]
  (GET :: () -> s -> a, PUT :: s -> a -> s)
  { PUT s (GET () s) => s
  , GET () (PUT s a) => a
  , PUT (PUT s a1) a2 => PUT s a2 }

= [Get + resulting state]
  (GET :: () -> s -> (a, s), PUT :: s -> a -> s)
  { PUT s (uncurry (\out _ -> out) (GET () s)) => s
  , uncurry (\out _ -> out) (GET () (PUT s a)) => a
  , PUT (PUT s a1) a2 => PUT s a2 }

= [Put + flip input]
  (GET :: () -> s -> (a, s), PUT :: a -> s -> s)
  { PUT (uncurry (\out _ -> out) (GET () s)) s => s
  , uncurry (\out _ -> out) (GET () (PUT a s)) => a
  , PUT a2 (PUT a1 s) => PUT a2 s }

= [Put + output]
  (GET :: () -> s -> (a, s), PUT :: a -> s -> ((), s))
  { uncurry (\_ s -> s) (PUT (uncurry (\out _ -> out) (GET () s)) s) => s
  , uncurry (\out _ -> out) (GET () (uncurry (\_ s -> s) (PUT a s))) => a
  , uncurry (\_ s -> s) (PUT a2 (uncurry (\_ s -> s) (PUT a1 s))) => uncurry (\_ s -> s) (PUT a2 s) }

= [def State]
  (GET :: () -> State s a, PUT :: a -> State s ())
  { uncurry (\_ s -> s) (PUT (uncurry (\out _ -> out) (GET () s)) s) => s
  , uncurry (\out _ -> out) (GET () (uncurry (\_ s -> s) (PUT a s))) => a
  , uncurry (\_ s -> s) (PUT a2 (uncurry (\_ s -> s) (PUT a1 s))) => uncurry (\_ s -> s) (PUT a2 s) }
```

## Abstracting

Some definitions:
```haskell
fmap :: (x -> y) -> State s x -> State s y
fmap f sx = s -> uncurry (\out s -> (f out, s)) (sx s)

(>>) :: State s x -> State s y -> State s y
sx >> sy = s -> uncurry (\_ s -> sy s) (sx s)

(>>=) :: State s x -> (x -> State s y) -> State s y
sx >>= f = s -> uncurry (\x s2 -> f x s2) (sx s)
```

And laws:
```haskell
uncurry g . second f  = uncurry (\x s -> g x (f s)) -- Uncurry 2
uncurry (f . g) = f . uncurry g                     -- Uncurry distribution
```

#### New laws

Contrived resulting state leads to this rule:
* Get is non-effectful: `uncurry (\_ s -> s) (GET s) = s`

Temptative law:
* Get is non-effectful: `get >> p = p`

#### 1st law

```haskell
  uncurry (\_ s -> s) (PUT (uncurry (\out _ -> out) (GET () s)) s) => s
= [Uncurry distribution]
  uncurry (\_ s -> s) (uncurry (\out _ -> PUT out s) (GET () s)) => s
= [def apply/abstract]
  uncurry (\_ s -> s) ((s -> uncurry (\out _ -> PUT out s) (GET () s)) s) => s
= [def return, def uncurry]
  uncurry (\_ s -> s) ((s -> uncurry (\out _ -> PUT out s) (GET () s)) s) => uncurry (\_ s -> s) (return ())
```

We can't reach `>>=` since PUT isn't using the state parameter. We need the new
law to do so.

```haskell
  uncurry (\_ s -> s) ((s -> uncurry (\out _ -> PUT out s) (GET () s)) s) => uncurry (\_ s -> s) (return ())
= [Non-effectful GET]
  uncurry (\_ s -> s) ((s -> uncurry (\out s -> PUT out s) (GET () s)) s) => uncurry (\_ s -> s) (return ())
= [def >>=]
  uncurry (\_ s -> s) ((GET () s >>= PUT) s) => uncurry (\_ s -> s) (return ())
= [output is ()]
  GET () s >>= PUT => return ()
```

Temptative law:
* `get () >>= put = return ()`

#### 2nd law

```haskell
  uncurry (\out _ -> out) (GET () (uncurry (\_ s -> s) (PUT a s))) => a
= [Uncurry distribution]
  uncurry (\out _ -> out) (uncurry (\_ s -> GET () s) (PUT a s)) => a
= [def apply/abstract]
  uncurry (\out _ -> out) ((s -> uncurry (\_ s -> GET () s) (PUT a s)) s) => a
= [def >>]
  uncurry (\out _ -> out) ((PUT a >> GET ()) s) => a
= [def return, def uncurry]
  uncurry (\out _ -> out) ((PUT a >> GET ()) s) => uncurry (\out _ -> out) (return a s)
= [Redundant PUT a]
  uncurry (\out _ -> out) ((PUT a >> GET ()) s) => uncurry (\out _ -> out) ((PUT a >> return a) s)
= [Non-effectful GET]
  (PUT a >> GET ()) s => (PUT a >> return a) s
```

Temptative law:
* `put a >> get () = put a >> return a`

#### 3rd law

```haskell
  uncurry (\_ s -> s) (PUT a2 (uncurry (\_ s -> s) (PUT a1 s))) => uncurry (\_ s -> s) (PUT a2 s)
= [Uncurry distribution]
  uncurry (\_ s -> s) (uncurry (\_ s -> PUT a2 s) (PUT a1 s)) => uncurry (\_ s -> s) (PUT a2 s)
= [def apply/abstract]
  uncurry (\_ s -> s) ((s -> uncurry (\_ s -> PUT a2 s) (PUT a1 s)) s) => uncurry (\_ s -> s) (PUT a2 s)
= [def >>]
  uncurry (\_ s -> s) ((PUT a1 >> PUT a2) s) => uncurry (\_ s -> s) (PUT a2 s)
= [output is ()]
  (PUT a1 >> PUT a2) s) => PUT a2 s
```

Temptative law:
* `put a1 >> put a2 = put a2`

## Lens algebra definition and laws

Lens algebra:
```haskell
LensAlg { get :: p a
        , set :: a -> p () }
```

With laws:
* Non-eff: `get >> p = p`
* GetPut: `get >>= put = return ()`
* PutGet: `put a >> get = put a >> return a`
* PutPut: `put a1 >> put a2 = put a2`

## What about `GetGet`?

Can we fulfill this law from the rest of them (including non-effectful get)?

```haskell
  get >>= (a1 -> get >>= (a2 -> p a1 a2)) => get >>= (a -> p a a)
= [p x y = q where q ignores (x, y)]
  get >>= (a1 -> get >>= (a2 -> q))
= [ignore parameters]
  get >>= (_ -> get >>= (_ -> q))
= [def >>]
  get >> get >> q
= [Non-effectful get]
  get >> q
= [def >>]
  get >>= (_ -> q)
= [p x y = q where q ignores (x, y)]
  get >>= (a -> p a a)
```

It works for a particular type of programs, those who ignore get outputs. We
can't say that GetGet is more general, since we can't define Non-effectful from
it.

However, these laws seem to have a relation at concrete level:

```haskell
  GET >>= (a1 -> GET >>= (a2 -> return (a1, a2))) => GET >>= (a -> return (a, a))
= [def >>=]
  s -> uncurry (\x s2 -> (a1 -> GET >>= (a2 -> return (a1, a2))) x s2) (GET s)
= [def >>=]
  s -> uncurry (\x s2 -> (a1 -> (s -> uncurry (\y s3 -> (a2 -> return (a1, a2)) y s3) (GET s))) x s2) (GET s)
= [def apply/abstract]
  s -> uncurry (\x s2 -> (a1 -> (s -> uncurry (\y s3 -> (return (a1, y)) s3) (GET s))) x s2) (GET s)
= [def apply/abstract]
  s -> uncurry (\x s2 -> (s -> uncurry (\y s3 -> (return (x, y)) s3) (GET s)) s2) (GET s)
= [Non-effectful GET s: s2 = s]
  s -> uncurry (\x _ -> (s -> uncurry (\y s3 -> (return (x, y)) s3) (GET s)) s) (GET s)
= [Non-effectful GET s: s3 = s]
  s -> uncurry (\x _ -> (s -> uncurry (\y _ -> (return (x, y)) s) (GET s)) s) (GET s)
= [def return, def apply]
  s -> uncurry (\x _ -> (s -> uncurry (\y _ -> ((x, y), s)) (GET s)) s) (GET s)
= [GET s = (x, _) => x = y]
  s -> uncurry (\x _ -> (s -> uncurry (\_ _ -> ((x, x), s)) (GET s)) s) (GET s)
= [Uncurry const]
  s -> uncurry (\x _ -> (s -> ((x, x), s)) s) (GET s)
= [def return]
  s -> uncurry (\x _ -> (return (x, x)) s) (GET s)
= [def abstract, Non-effectful GET]
  s -> uncurry (\x s2 -> (return (x, x)) s2) (GET s)
= [def abstract]
  s -> uncurry (\x s2 -> (\x -> return (x, x)) x s2) (GET s)
= [def >>=]
  GET >>= (x -> return (x, x))
= [variable renaming]
  GET >>= (a -> return (a, a))
```

**Anyway, reaching GetGet is still an open question!!!**
