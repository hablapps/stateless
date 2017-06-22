{-# LANGUAGE FlexibleContexts #-}

module Stateless where

import Data.Functor

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

--------------------------------
-- Classic Optics (with laws) --
--------------------------------

data Lens' s a = Lens' { get :: s -> a, put :: a -> s -> s }

prop_PutGet l s a     = get l (put l s a) == a
prop_GetPut l s       = put l (get s) s   == s
prop_PutPut l s a1 a2 = (put l a1 . put l a2) s == put l a2 s

data Affine' s a = Affine' { getMaybe :: s -> Maybe a
                           , put' :: a -> s -> s }

prop_PutGetm af s a      = getMaybe af (put' af a s) == (getMaybe af s $> a)
prop_GetPutm af s        = fmap (\a -> put' af a s) (getMaybe af s) == getMaybe af s
prop_PutmPutm af s a1 a2 = (put' af a1 . put' af a2) s == put' af a1 s

newtype Getter' s a = Getter' { get' :: s -> a }

-- no Getter laws

newtype Fold' s a = Fold' { getList :: s -> [a] }

-- no Fold laws

newtype Setter' s a = Setter' { modify :: (a -> a) -> s -> s }

prop_ModId  st     s = modify st id s == s
prop_ModCom st f g s = (modify st f . modify st g) s == modify st (f . g) s

data Traversal' s a = Traversal' { getList' :: s -> [a]
                                 , putList :: [a] -> s -> s }

prop_PutlGetl t s a     = getList' t (putList t s a) == a
prop_GetlPutl t s       = putList t (getList' t s) s == s
prop_PutlPutl t s as1 as2 = (putList t as1 . putList t as2) s == putList t as2 s

--------------------
-- Optic Algebras --
--------------------

-- Raw representation

class LensAlg f where
  la_get :: f a
  la_put :: a -> f ()

-- Natural representation
