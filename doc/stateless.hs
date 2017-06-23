{-# LANGUAGE FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , TypeFamilies
  , TypeOperators #-}

module Stateless where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Data.Functor

-- -------------------------
-- -- Optics as Machines! --
-- -------------------------
--
-- -- data Iso s a = Iso { runIso :: s -> (a, a -> s) }
-- newtype Lens s a = Lens { runLens :: s -> (a, a -> s) }
--
-- -- newtype Prism s a = Prism { runPrism :: s -> (Maybe a, a -> s) }
-- newtype Affine s a = Affine { runAffine :: s -> (Maybe a, a -> s) }
--
-- newtype Getter s a = Getter { runGetter :: s -> a }
--
-- newtype Fold s a = Fold { runFold :: s -> [a] }
--
-- newtype Setter s a = Setter { runSetter :: s -> (a -> a) -> s }
--
-- -- for a fixed size N
-- newtype Traversal s a = Traversal { runTraversal :: s -> ([a], [a] -> s) }
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

-- Raw representation

-- MonadState
-- class LensAlg p a where
--   la_get :: p a
--   la_put :: a -> p ()

-- Natural representation

class (Monad p, MonadState a (Q a)) => LensAlg p a where
  data Q a :: * -> *
  hom :: Q a ~> p

  get' :: p a
  get' = hom get
  put' :: a -> p ()
  put' a = hom (put a)
  mod' :: (a -> a) -> p ()
  mod' f = get' >>= (put' . f)

instance Functor (Q Int) where
  fmap f (Wrapper s) = Wrapper (fmap f s)

instance Applicative (Q Int) where
  pure = Wrapper . pure
  (Wrapper s) <*> (Wrapper f) = Wrapper $ s <*> f

instance Monad (Q Int) where
  return = Wrapper . return
  (Wrapper s) >>= f = Wrapper $ s >>= (runWrapper . f)

instance MonadState Int (Q Int) where
  get = Wrapper get
  put a = Wrapper (put a)

data Person = Person { name :: String, age :: Int } deriving Show

instance LensAlg (State Person) Int where
  data Q Int b = Wrapper { runWrapper :: State Int b }
  hom qix = StateT (\p -> Identity (
    runIdentity $ evalStateT (runWrapper qix) (age p),
    p { age = runIdentity $ execStateT (runWrapper qix) (age p) }))

getAge :: Person -> Int
getAge p = runIdentity $ evalStateT get' p

putAge :: Person -> Int -> Person
putAge p a = runIdentity $ execStateT (put' a) p

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

maybeToList :: Maybe ~> []
maybeToList (Just a) = [a]
maybeToList Nothing = []

-- Type families

data family MyFamily a :: *
data instance MyFamily () = Jesus
data instance MyFamily Char = Godfather Int Double | Father String

familyFun :: MyFamily () -> Bool
familyFun Jesus = True

familyFun' :: MyFamily Char -> Bool
familyFun' (Godfather _ _) = False
familyFun' (Father _) = False
