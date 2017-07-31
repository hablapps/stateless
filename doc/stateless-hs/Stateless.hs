{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Stateless where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Constant
import Data.HList.HList
import Data.Monoid

--------------------
-- Optic Algebras --
--------------------

type OpticAlg p q f = (Monad p, Monad q, Functor f) =>
                      forall x. q x -> p (f x)

type LensAlg p q a = (MonadState a q) => OpticAlg p q Identity

type TraversalAlg p q a = (MonadState a q) => OpticAlg p q []

type AffineAlg p q a = (MonadState a q) => OpticAlg p q Maybe

type SetterAlg p q a = (MonadState a q) => OpticAlg p q (Constant ())

-- XXX: `MonadAsk` indeed
type GetterAlg p q a = (MonadReader a q) => OpticAlg p q Identity

type FoldAlg p q a = (MonadReader a q) => OpticAlg p q []

-- indexed

type IOpticAlg i p q f = (Monad p, Monad q, Functor f) =>
                         forall x. (i -> q x) -> p (f x)

type ITraversalAlg i p q a = (MonadState a q) => IOpticAlg i p q []

-- composition

compose :: (Monad q, Functor g) =>
           (forall x. f (g x) -> f x) ->
           OpticAlg p q f ->
           OpticAlg q r g ->
           OpticAlg p r f
compose j o1 o2 = fmap j . o1 . o2

composeM :: (Monad q, Monad m) =>
            OpticAlg p q m ->
            OpticAlg q r m ->
            OpticAlg p r m
composeM = compose join

(^|->) :: (Monad p, Monad q, MonadState a q, MonadState b r) =>
          TraversalAlg p q a ->
          LensAlg q r b ->
          TraversalAlg p r b
(^|->) = compose (fmap runIdentity)

asIndexed :: OpticAlg p q f -> IOpticAlg (HList '[]) p q f
asIndexed op f = op $ f HNil

fromIndexed :: IOpticAlg (HList '[]) p q f -> OpticAlg p q f
fromIndexed iop qx = iop (const qx)

-- indexed composition

icompose :: (Functor p, Monad q, Functor g, HAppendList l1 l2) =>
            (forall x. f (g x) -> f x) ->
            IOpticAlg (HList l1) p q f ->
            IOpticAlg (HList l2) q r g ->
            IOpticAlg (HList (HAppendListR l1 l2)) p r f
icompose j iop1 iop2 f = j <$> iop1 (\i -> iop2 (f . hAppendList i))

icomposeM :: (Functor p, Monad q, Monad m, HAppendList l1 l2) =>
             IOpticAlg (HList l1) p q m ->
             IOpticAlg (HList l2) q r m ->
             IOpticAlg (HList (HAppendListR l1 l2)) p r m
icomposeM = icompose join

-- XXX: `HAppendListR l '[]` is not returning `l`!
(~^|->) :: (Monad p, Monad q, MonadState a q, MonadState b r, HAppendList l '[]) =>
           ITraversalAlg (HList l) p q a ->
           LensAlg q r b ->
           ITraversalAlg (HList (HAppendListR l '[])) p r b
(~^|->) itr ln = icompose (fmap runIdentity) itr (asIndexed ln)

-- functions

view :: (Monad p, MonadState a q, Functor f) => OpticAlg p q f -> p (f a)
view op = op get

-- XXX: this redundancy could be removed if we had `MonadState` depending on
-- `MonadAsk` and `MonadPut`
view' :: (Monad p, MonadReader a q, Functor f) => OpticAlg p q f -> p (f a)
view' op = op ask

set :: (Monad p, MonadState a q, Functor f) => OpticAlg p q f -> a -> p (f ())
set op a = op $ put a

modi :: (Monad p, MonadState a q, Functor f) =>
        OpticAlg p q f -> (a -> a) -> p (f ())
modi op f = op $ modify f

fold :: (Monad p, MonadState a q, Foldable f, Functor f, Monoid m) =>
        OpticAlg p q f -> (a -> m) -> p m
fold op f = fmap (foldMap f) (view op)

-- indexed functions

-- XXX: where's strength? (category-extras is obsolete!?!?)
iview :: (Monad p, MonadState a q, Functor f) =>
         IOpticAlg i p q f -> p (f (i, a))
iview op = op (\i -> fmap ((,) i) get)

iindex :: (Monad p, MonadState a q, Functor f) => IOpticAlg i p q f -> p (f i)
iindex = fmap (fmap fst) . iview

ifoci :: (Monad p, MonadState a q, Functor f) => IOpticAlg i p q f -> p (f a)
ifoci = fmap (fmap snd) . iview

ifold :: (Monad p, MonadState a q, Functor f, Foldable f, Monoid m) =>
         IOpticAlg i p q f -> ((i, a) -> m) -> p m
ifold iop f = fmap (foldMap f) (iview iop)

icontains :: (Monad p, MonadState a q, Functor f, Foldable f, Eq i) =>
             IOpticAlg i p q f -> i -> p Bool
icontains iop i = getAny <$> ifold iop (\(i2, _) -> Any $ i == i2)

imodi :: (Monad p, MonadState a q, Functor f) =>
         IOpticAlg i p q f -> (i -> a -> a) -> p (f ())
imodi iop f = iop (modify . f)

ifilter :: (Monad p, MonadState a q, Eq i, FilterIndex i p q a) =>
           (i -> Bool) -> q x -> p [x]
ifilter p qx = filterIndex p (const qx)

ipick :: (Monad p, MonadState a q, Eq i, FilterIndex i p q a) =>
         i -> q x -> p [x]
ipick i = ifilter (== i)

-- exotic composition

parLens :: LensAlg p q a -> LensAlg p q a -> TraversalAlg p q a
parLens l1 l2 q = (\(Identity a1) (Identity a2) -> [a1, a2]) <$> l1 q <*> l2 q

xcompose :: (Monad q, Functor g) =>
            (forall x. f (g x) -> f x) ->
            IOpticAlg (HList l) p q f ->
            OpticAlg q r g ->
            IOpticAlg (HList l) p r f
xcompose j iop op rx = j <$> iop (op . rx)

-- Ops

newtype At' i p q a = At' { runAt' :: i -> LensAlg p q (Maybe a) }

class At i p q a | p -> q, q -> a where
  at :: i -> LensAlg p q (Maybe a)

class FilterIndex i p q a | p -> q, q -> a where
  filterIndex :: (i -> Bool) -> ITraversalAlg i p q a

-------------------------
-- Optics as Machines! --
-------------------------

-- data Iso s a = Iso { runIso :: s -> (a, a -> s) }
newtype Lens s a = Lens { runLens :: s -> (a, a -> s) }

-- newtype Prism s a = Prism { runPrism :: s -> (Maybe a, a -> s) }
newtype Affine s a = Affine { runAffine :: s -> (Maybe a, a -> s) }

newtype Getter s a = Getter { runGetter :: s -> a }

newtype Fold s a = Fold { runFold :: s -> [a] }

newtype Setter s a = Setter { runSetter :: s -> (a -> a) -> s }

-- for a fixed size N
newtype Traversal s a = Traversal { runTraversal :: s -> ([a], [a] -> s) }

-- Weak Traversal
newtype WkTraversal s a = WkTraversal { runWkTraversal :: s -> ([a], (a -> a) -> s) }

-- Lens Iso

fromLn :: Lens s a -> LensAlg (State s) (State a) a
fromLn ln sa = StateT (\s -> let (a, f)  = runLens ln s
                                 (x, a2) = runState sa a
                             in Identity (Identity x, f a2))

toLn :: LensAlg (State s) (State a) a -> Lens s a
toLn lna = Lens (\s -> (runIdentity $ evalState (lna get) s,
                        \a -> execState (lna $ put a) s))

-- Getter Iso

fromGt :: Getter s a -> GetterAlg (Reader s) (Reader a) a
fromGt gt ra = ReaderT (Identity . Identity . runReader ra .runGetter gt)

toGt :: GetterAlg (Reader s) (Reader a) a -> Getter s a
toGt gta = Getter (runIdentity . runReader (gta ask))

-- Fold Iso

fromFl :: Fold s a -> FoldAlg (Reader s) (Reader a) a
fromFl fl ra = ReaderT (Identity . fmap (runReader ra) . runFold fl)

toFl :: FoldAlg (Reader s) (Reader a) a -> Fold s a
toFl fla = Fold (runReader (fla ask))

-- Setter Iso

fromSt :: Setter s a -> SetterAlg (State s) (State a) a
fromSt st sa = StateT (\s -> Identity (Constant (), runSetter st s (execState sa)))

toSt :: SetterAlg (State s) (State a) a -> Setter s a
toSt sta = Setter (\s f -> execState (sta (modify f)) s)

-- Affine Iso

fromAf :: Affine s a -> AffineAlg (State s) (State a) a
fromAf af sa = StateT (\s -> let (ma, f) = runAffine af s
                                 mxa2 = fmap (runState sa) ma
                             in Identity (fmap fst mxa2, maybe s (f . snd) mxa2))

toAf :: AffineAlg (State s) (State a) a -> Affine s a
toAf afa = Affine (\s -> (evalState (afa get) s,
                          \a -> execState (afa $ put a) s))

-- Traversal Iso can't be defined, `TraversalAlg` is weaker than `Traversal`

fromTr :: Traversal s a -> TraversalAlg (State s) (State a) a
fromTr tr sa = StateT (\s -> let (as, f) = runTraversal tr s
                                 xa2s = fmap (runState sa) as
                             in Identity (fmap fst xa2s, f (fmap snd xa2s)))

-- XXX: `TraversalAlg` is weaker than `Traversal`!
toTr :: TraversalAlg (State s) (State a) a -> Traversal s a
toTr tra = Traversal (\s -> (evalState (tra get) s, undefined))

-- Weak Travesal Iso

fromWTr :: WkTraversal s a -> TraversalAlg (State s) (State a) a
fromWTr tr sa = StateT (\s -> let (as, f) = runWkTraversal tr s
                                  xs = fmap (evalState sa) as
                              in Identity (xs, f (execState sa)))

toWTr :: TraversalAlg (State s) (State a) a -> WkTraversal s a
toWTr tra = WkTraversal (\s -> (evalState (tra get) s,
                                \f -> execState (tra $ modify f) s))

------------------
-- Raw Algebras --
------------------

-- lens

class (Monad p) => RawLensAlg p a | p -> a where
  lnGet :: p a
  lnPut :: a -> p ()

data RawLensAlg' p a b where
  LnGet :: RawLensAlg' p a a
  LnPut :: a -> RawLensAlg' p a ()
  LnBind :: p x -> (x -> p y) -> RawLensAlg' p a y
  LnReturn :: x -> RawLensAlg' p a x

-- getter

class (Monad p) => RawGetterAlg p a | p -> a where
  gtGet :: p a

data RawGetterAlg' p a b where
  GtGet :: RawGetterAlg' p a [a]
  GtBind :: p x -> (x -> p y) -> RawGetterAlg' p a y
  GtReturn :: x -> RawGetterAlg' p a x

-- setter

class (Monad p) => RawSetterAlg p a | p -> a where
  stMod :: (a -> a) -> p ()

data RawSetterAlg' p a b where
  StMod :: (a -> a) -> RawSetterAlg' p a ()
  StBind :: p x -> (x -> p y) -> RawSetterAlg' p a y
  StReturn :: x -> RawSetterAlg' p a x

-- traversal

class (Monad p) => RawTraversalAlg p a | p -> a where
  trGet :: p [a]
  trMod :: (a -> a) -> p ()

data RawTraversalAlg' p a b where
  TrGet :: RawTraversalAlg' p a [a]
  TrMod :: (a -> a) -> RawTraversalAlg' p a ()
  TrBind :: p x -> (x -> p y) -> RawTraversalAlg' p a y
  TrReturn :: x -> RawTraversalAlg' p a x

-- fold

class (Monad p) => RawFoldAlg p a | p -> a where
  flGet :: p [a]

data RawFoldAlg' p a b where
  FlGet :: RawFoldAlg' p a (Maybe a)
  FlBind :: p x -> (x -> p y) -> RawFoldAlg' p a y
  FlReturn :: x -> RawFoldAlg' p a x

-- affine

class (Monad p) => RawAffineAlg p a | p -> a where
  afGet :: p (Maybe a)
  afPut :: a -> p ()

data RawAffineAlg' p a b where
  AfGet :: RawAffineAlg' p a (Maybe a)
  AfPut :: a -> RawAffineAlg' p a ()
  AfBind :: p x -> (x -> p y) -> RawAffineAlg' p a y
  AfReturn :: x -> RawAffineAlg' p a x

-- generic algebra

class (Monad p, Functor f) => ReadOpticAlg p f a where
  rOpGet :: p (f a)

class (Monad p, Functor f) => WriteOpticAlg p f a where
  wOpMod :: (a -> a) -> p (f ())

class (Monad p, Functor f) => ReadWriteOpticAlg p f a where
  rwOpGet :: p (f a)
  rwOpMod :: (a -> a) -> p (f ())

-- generic algebra instances

instance (RawGetterAlg p a) => ReadOpticAlg p Identity a where
  rOpGet = fmap Identity gtGet

instance (RawFoldAlg p a) => ReadOpticAlg p [] a where
  rOpGet = flGet

instance (RawSetterAlg p a) => WriteOpticAlg p (Constant ()) a where
  wOpMod = fmap Constant . stMod

instance (RawLensAlg p a) => ReadWriteOpticAlg p Identity a where
  rwOpGet = fmap Identity lnGet
  rwOpMod f = lnGet >>= (fmap Identity . lnPut . f)

instance (RawAffineAlg p a) => ReadWriteOpticAlg p Maybe a where
  rwOpGet = afGet
  rwOpMod f = afGet >>= maybe (return Nothing) (fmap Just . afPut . f)

instance (RawTraversalAlg p a) => ReadWriteOpticAlg p [] a where
  rwOpGet = trGet
  rwOpMod f = trMod f >> fmap void trGet

-- --------------------------------
-- -- Classic Optics (with laws) --
-- --------------------------------
--
-- data Lens' s a = Lens' { get :: s -> a, put :: a -> s -> s }
--
-- prop_PutGet l s a     = get l (put l s a) == a
-- prop_GetPut l s       = put l (get s) s   == s
-- prop_PutPut l s a1 a2 = (put l a1 . put l a2) s == put l a2 s
--
-- data Affine' s a = Affine' { getMaybe :: s -> Maybe a
--                            , put' :: a -> s -> s }
--
-- prop_PutGetm af s a      = getMaybe af (put' af a s) == (getMaybe af s $> a)
-- prop_GetPutm af s        = fmap (\a -> put' af a s) (getMaybe af s) == getMaybe af s
-- prop_PutmPutm af s a1 a2 = (put' af a1 . put' af a2) s == put' af a1 s
--
-- newtype Getter' s a = Getter' { get' :: s -> a }
--
-- -- no Getter laws
--
-- newtype Fold' s a = Fold' { getList :: s -> [a] }
--
-- -- no Fold laws
--
-- newtype Setter' s a = Setter' { modify :: (a -> a) -> s -> s }
--
-- prop_ModId  st     s = modify st id s == s
-- prop_ModCom st f g s = (modify st f . modify st g) s == modify st (f . g) s
--
-- data Traversal' s a = Traversal' { getList' :: s -> [a]
--                                  , putList :: [a] -> s -> s }
--
-- prop_PutlGetl t s a     = getList' t (putList t s a) == a
-- prop_GetlPutl t s       = putList t (getList' t s) s == s
-- prop_PutlPutl t s as1 as2 = (putList t as1 . putList t as2) s == putList t as2 s
