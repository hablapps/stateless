{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Stateless where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Constant
import Data.HList.HList
import Data.Map
import Data.Monoid
import GHC.TypeLits

-- -------------------------
-- -- Optics as Machines! --
-- -------------------------

-- data Iso s a = Iso { runIso :: s -> (a, a -> s) }
newtype Lens s a = Lens { runLens :: s -> (a, a -> s) }

-- newtype Prism s a = Prism { runPrism :: s -> (Maybe a, a -> s) }
newtype Affine s a = Affine { runAffine :: s -> (Maybe a, a -> s) }

newtype Getter s a = Getter { runGetter :: s -> a }

newtype Fold s a = Fold { runFold :: s -> [a] }

newtype Setter s a = Setter { runSetter :: s -> (a -> a) -> s }

-- for a fixed size N
newtype Traversal s a = Traversal { runTraversal :: s -> ([a], [a] -> s) }
--
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

--------------------
-- Optic Algebras --
--------------------

-- Natural Representation (Experimental, No Typeclasses)

-- XXX: what are the implications of type constraints?

type OpticAlg p q f = forall x. q x -> p (f x)

type LensAlg p q a = (Monad p, MonadState a q) => OpticAlg p q Identity

type GetterAlg p q a = (Monad p, MonadReader a q) => OpticAlg p q Identity

type TraversalAlg p q a = (Monad p, MonadState a q) => OpticAlg p q []

type FoldAlg p q a = (Monad p, MonadReader a q) => OpticAlg p q []

type AffineAlg p q a = (Monad p, MonadState a q) => OpticAlg p q Maybe

type SetterAlg p q a = (Monad p, MonadState a q) => OpticAlg p q (Constant ())

-- indexed

type IOpticAlg i p q f = forall x. (i -> q x) -> p (f x)

type ITraversalAlg i p q a = (Monad p, MonadState a q) => IOpticAlg i p q []

-- composition

compose :: (Functor p) =>
           (forall x. f (g x) -> f x) ->
           OpticAlg p q f ->
           OpticAlg q r g ->
           OpticAlg p r f
compose j o1 o2 = fmap j . o1 . o2

composeM :: (Functor p, Monad m) =>
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

icompose :: (Functor p, HAppendList l1 l2) =>
            (forall x. f (g x) -> f x) ->
            IOpticAlg (HList l1) p q f ->
            IOpticAlg (HList l2) q r g ->
            IOpticAlg (HList (HAppendListR l1 l2)) p r f
icompose j iop1 iop2 f = j <$> iop1 (\i -> iop2 (f . hAppendList i))

-- XXX: `HAppendListR l '[]` is not returning `l`!
(^^|->) :: (Monad p, Monad q, MonadState a q, MonadState b r, HAppendList l '[]) =>
           ITraversalAlg (HList l) p q a ->
           LensAlg q r b ->
           ITraversalAlg (HList (HAppendListR l '[])) p r b
(^^|->) itr ln = icompose (fmap runIdentity) itr (asIndexed ln)

-- functions

view :: (MonadState a q) => OpticAlg p q f -> p (f a)
view op = op get

set :: (MonadState a q) => OpticAlg p q f -> a -> p (f ())
set op a = op $ put a

modi :: (MonadState a q) => OpticAlg p q f -> (a -> a) -> p (f ())
modi op f = op $ modify f

fold :: (Functor p, MonadState a q, Foldable f, Monoid m) =>
        OpticAlg p q f -> (a -> m) -> p m
fold op f = fmap (foldMap f) (view op)

-- indexed functions

-- XXX: where's strength? (category-extras is obsolete!?!?)
iview :: (MonadState a q) => IOpticAlg i p q f -> p (f (i, a))
iview op = op (\i -> fmap ((,) i) get)

iindex :: (Functor p, Functor f, MonadState a q) => IOpticAlg i p q f -> p (f i)
iindex = fmap (fmap fst) . iview

ifold :: (Functor p, MonadState a q, Foldable f, Monoid m) =>
         IOpticAlg i p q f -> ((i, a) -> m) -> p m
ifold iop f = fmap (foldMap f) (iview iop)

imodi :: (MonadState a q) => IOpticAlg i p q f -> (a -> a) -> p (f ())
imodi iop f = iop (const $ modify f)

-- university

class UniversityView p where
  universityName :: p String
  departmentNames :: p [String]
  totalBudget :: p Int
  duplicateBudget :: p ()

class UniversityData p q d | p -> q, q -> p, q -> d where
  name :: LensAlg p (State String) String
  departments :: ITraversalAlg (HList '[String]) p q d
  budget :: LensAlg q (State Int) Int

-- XXX: requires undecidable and ambiguous
instance (Monad p, Monad q, MonadState d q, UniversityData p q d) => UniversityView p where
  universityName = runIdentity <$> view name
  departmentNames = fmap hHead <$> iindex departments
  totalBudget = getSum <$> ifold (departments ^^|-> budget) (Sum . snd)
  duplicateBudget = void $ imodi (departments ^^|-> budget) (* 2)

-- instantiating university

data University = University { nm :: String, deps :: Map String Department } deriving Show
newtype Department = Department { bud :: Int } deriving Show

instance UniversityData (State University) (State Department) Department where
  name sn = StateT (\u -> let (a, nm2) = runState sn (nm u) in
    Identity (Identity a, u { nm = nm2 }))
  departments sd =
    StateT (\u ->
      let (as, deps2) = unzip $ fmap (\(k, d) -> fmap ((,) k) (runState (sd (k `HCons` HNil)) d)) (assocs $ deps u) in
        Identity (as, u { deps = fromList deps2 }))
  budget sb = StateT (\d -> let (a, bud2) = runState sb (bud d) in
    Identity (Identity a, d { bud = bud2 }))

main :: IO ()
main = do
  putStrLn "URJC budget: "
  let uni = University "URJC" (fromList [("CS", Department 1000), ("HS", Department 2000)])
  putStr "* Total budget: "
  print $ evalState totalBudget uni
  putStr "* After duplicating budget: "
  print $ runState duplicateBudget uni
  putStr "* Department names: "
  print $ evalState departmentNames uni

-- End of experimental

-- class (Monad p, MonadState (Aln q) q) => LensAlg p q where
--   type Aln q
--   lnHom :: q ~> p
--
-- class (Monad p, MonadState (Atr q) q) => TraversalAlg p q where
--   type Atr q
--   trHom :: q x -> p [x]
--
-- instance (LensAlg p q, LensAlg q r) => LensAlg p r where
--   type Aln r
--   lnHom = undefined

-- -- Natural representation
--
-- -- XXX: `p -> q` is necessary to get things working in Haskell, but we don't
-- -- want stateless to behave like that. In fact, we want heterogeneity!
--
-- class (Monad p, MonadState a q) => NatLensAlg p q a where
--   lnHom :: q ~> p
--
-- class (Monad p, MonadState a q) => NatTraversalAlg p q a where
--   trHom :: forall x. q x -> p [x]
--
--   trGet :: p [a]
--   trGet = trHom get
--   trFoldMap :: (Monoid m) => (a -> m) -> p m
--   trFoldMap f = fmap (foldMap f) trGet
--
-- class (Monad p, MonadReader a q) => NatGetterAlg p q a | p -> q where
--   gtHom :: q ~> p
--
-- class (Monad p, MonadReader a q) => NatFoldAlg p q a | p -> q where
--   flHom :: forall x. q x -> p [x]
--
-- class (Monad p, MonadState a q) => NatAffineAlg p q a | p -> q where
--   afHom :: forall x. q x -> p (Maybe x)
--
-- class (Monad p, MonadState a q) => NatSetterAlg p q a | p -> q where
--   stHom :: forall x. q x -> p (Constant () x)
--
-- ------------------
-- -- Composition ---
-- ------------------
--
-- -- XXX: we are limited in the number of instances we can create, this is
-- -- Haskell! Therefore we need a bunch of newtypes to make instances distinguible
-- -- from each other
--
-- newtype LL a = LL { getLL :: a }
-- newtype LG a = LG { getLG :: a }
-- newtype LS a = LS { getLS :: a }
-- newtype TL a = TL { getTL :: a }
-- newtype LT a = LT { getLT :: a }
--
-- -- XXX: this is a nonsense. If `p -> q` is a dependency, then `p` determines a
-- -- unique `q`. Then, we say that there's a valid instance for `p` and `r`, so
-- -- `p -> r`.
-- instance (NatLensAlg p q a, NatLensAlg q r (LL b)) => NatLensAlg p r (LL b) where
--   lnHom = lnHom . lnHom
--
-- instance (NatLensAlg p q a, NatTraversalAlg q r (LT b)) => NatTraversalAlg p r (LT b) where
--   trHom = lnHom . trHom
--
-- instance (NatTraversalAlg p q a, NatLensAlg q r (TL b)) => NatTraversalAlg p r (TL b) where
--   trHom = trHom . lnHom
--
-- instance (NatLensAlg p q a, NatGetterAlg q r (LG b)) => NatGetterAlg p r (LG b) where
--   gtHom = lnHom . gtHom
--
-- instance (NatLensAlg p q a, NatSetterAlg q r (LS b)) => NatSetterAlg p r (LS b) where
--   stHom = lnHom . stHom
--
-- ----------------
-- -- Nat 2 Raw ---
-- ----------------
--
-- instance (NatLensAlg p q a) => RawLensAlg p a where
--   la_get = lnHom get
--   la_put a = lnHom (put a)
--
-- instance (NatTraversalAlg p q a) => RawTraversalAlg p a where
--   tr_get = trHom get
--   tr_modify f = void $ trHom (modify f)
--
-- -- XXX: What is "UndecidableInstances" extension? I need it to keep instances
-- -- happy!
--
-- -----------------
-- -- University ---
-- -----------------
--
-- -- view
--
-- type Name = String
-- type Budget = Int
--
-- class University p d where
--   name :: p Name
--   removeDepartment :: Name -> p ()
--   addDepartment :: Name -> Int -> [(String, String, Int)] -> p ()
--   updateSalary :: (Int -> Int) -> p ()
--   upperCase :: p ()
--   containsDepartment :: String -> p Bool
--   getTotalSalary :: p Int
--   getNameInitials :: p String
--   getTotalBudget :: p Budget
--   getBudget :: String -> p (Maybe Budget)
--
-- -- data
--
-- instance (NatLensAlg p (State Name) Name,
--           NatTraversalAlg p q d,
--           NatLensAlg q (State (TL Budget)) (TL Budget)) => University p d where
--   name = la_get
--   getTotalBudget =
--     -- ERROR: fmap getSum (tr_foldMap (Sum . getTL))
--     -- fmap getSum (trFoldMap (Sum . getTL))
--     fmap (getSum . foldMap (Sum . getTL)) (trHom (get :: State (TL Budget) (TL Budget)))
--
-- --------------------------
-- -- University Instance ---
-- --------------------------
--
-- -- TODO: State, other effects? bbdd libraries...

--------------
-- Sandbox ---
--------------

-- Typeclass Inheritance

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- class (Stateless.Functor m) => Monad m where
--   return :: a -> m a
--   bind :: (a -> m b) -> m a -> m b

-- class (Monad m) => MonadState m a where
--   ms_get :: m a
--   ms_put :: a -> m ()
--
-- instance Monad (State s) where
--   return = undefined
--   (>>=) = undefined
--
-- newtype State s a = State { runState :: s -> (s, a) }
--
-- instance MonadState (State s) a where
--   ms_get = undefined
--   ms_put = undefined

-- Natural transformations

type (~>) f g = forall a . f a -> g a

-- Type families

data family MyFamily a :: *
data instance MyFamily () = Jesus
data instance MyFamily Char = Godfather Int Double | Father String

familyFun :: MyFamily () -> Bool
familyFun Jesus = True

familyFun' :: MyFamily Char -> Bool
familyFun' (Godfather _ _) = False
familyFun' (Father _) = False

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'False c      = 'False
  And 'True  d      = d
  And e      'False = 'False
  And f      'True  = f
  And g      g      = g

data Vector :: Nat -> * -> * where
  VNil :: Vector 0 a
  (:*:) :: a -> Vector n a -> Vector (n + 1) a

infixr 5 :*:

unsafeHead :: Vector n a -> a
unsafeHead VNil = undefined
unsafeHead (a :*: _) = a

type family NonZero (a :: Nat) :: Bool where
  NonZero 0 = 'False
  NonZero x = 'True

safeHead :: NonZero n ~ 'True => Vector n a -> a
safeHead (a :*: _) = a

v :: Vector 3 Int
v = 1 :*: 2 :*: 3 :*: VNil

x :: Int
x = safeHead v

newtype I f = I f
newtype C f = C f
data S f = S String f

formatSpec :: S (I (S (C String)))
formatSpec = S "Int: " $ I $ S ", Char: " $ C "."

class Format fmt where
  type Sprintf fmt
  sprintf' :: String -> fmt -> Sprintf fmt

instance Format String where
  type Sprintf String = String
  sprintf' pre str = pre ++ str

instance Format a => Format (I a) where
  type Sprintf (I a) = Int -> Sprintf a
  sprintf' pre (I a) i = sprintf' (pre ++ show i) a

instance Format a => Format (C a) where
  type Sprintf (C a) = Char -> Sprintf a
  sprintf' pre (C a) c = sprintf' (pre ++ show c) a

instance Format a => Format (S a) where
  type Sprintf (S a) = Sprintf a
  sprintf' pre (S s a) = sprintf' (pre ++ s) a

sprintf :: Format fmt => fmt -> Sprintf fmt
sprintf = sprintf' ""

xyz :: Int -> Char -> String
xyz = sprintf formatSpec

-- Functional dependencies paper

class Collects ce e | ce -> e where
  empty  :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool
  def    :: e

  -- -- This can't be done, since `def` can't uniquely determine `ce`
  -- insertDef :: ce -> ce
  -- insertDef = insert def

class Collects2 ce where
  type E ce
  empty2  :: ce
  insert2 :: E ce -> ce -> ce
  member2 :: E ce -> ce -> Bool
  def2    :: E ce

  -- -- Same here, `E ce` can't determine `ce` by its own.
  -- insertDef2 :: ce -> ce
  -- insertDef2 = insert2 def2

class Monoide m where
  mappend :: m -> m -> m
  mzero :: m

instance Monoide Int where
  mappend = (+)
  mzero = 0

newtype Int' = Go Int deriving Monoide
