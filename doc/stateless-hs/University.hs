{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module University where

import Prelude hiding (last)

import Control.Monad.Identity
import Control.Monad.State
import Data.Char
import Data.HList.HList
import Data.Map
import Data.Maybe
import Data.Monoid
import Stateless

-- university

class UniversityView p where
  uniName :: p String
  departmentNames :: p [String]
  totalBudget :: p Int
  getBudget :: String -> p (Maybe Int)
  duplicateBudget :: p ()
  totalSalary :: p Int
  updateSalary :: (Int -> Int) -> p ()
  upperCase :: p ()
  nameInitials :: p String -- XXX: this method is so weird!
  containsDepartment :: String -> p Bool
  addDepartment :: String -> Int -> [(String, String, Int)] -> p ()
  remDepartment :: String -> p ()

-- XXX: Yo dawg, I put a dependency in your dependency so you can depend on a
-- dependency while you depend on dependencies. So... do these dependencies
-- make any sense?
class (Monad p, Monad q, Monad r,
       MonadState (D q) q, MonadState (L r) r, MonadState (Maybe (D q)) s) =>
      UniversityData p q r s | p -> q, q -> r, r -> q, q -> p, q -> s where
  type D q :: *
  type L r :: *
  name :: LensAlg p (State String) String
  departments :: ITraversalAlg (HList '[String]) p q (D q)
  department :: String -> LensAlg p s (Maybe (D q))
  budget :: LensAlg q (State Int) Int
  lecturers :: ITraversalAlg (HList '[String]) q r (L r)
  first :: LensAlg r (State String) String
  last :: LensAlg r (State String) String
  salary :: LensAlg r (State Int) Int
  -- XXX: `p` is required to enable typeclass inference
  mkDepartment :: Int -> [(String, String, Int)] -> p (D q)
  mkLecturer :: (String, String, Int) -> p (L r)

  -- XXX: do these compositions belong here? I find them interesting in order to
  -- reuse them at view instances, but they seem quite adhoc for the particular
  -- views. Furthermore, I'm finding inference problems while combining directly
  -- on the instance (UndecidableInstances?).
  uniBudget :: ITraversalAlg (HList '[String]) p (State Int) Int
  uniBudget = departments ~^|-> budget
  uniOneBudget :: (FilterIndex (HList '[String]) p q (D q)) =>
                  String -> ITraversalAlg (HList '[String]) p (State Int) Int
  uniOneBudget s = filterIndex (\i -> i == (s `HCons` HNil)) ~^|-> budget
  uniSalary :: ITraversalAlg (HList '[String, String]) p (State Int) Int
  uniSalary = uniLecturers ~^|-> salary
  uniLecturers :: ITraversalAlg (HList '[String, String]) p r (L r)
  uniLecturers = icomposeM departments lecturers
  lecNames :: TraversalAlg r (State String) String
  lecNames = first `parLens` last
  lecHeadNames :: TraversalAlg r (State Char) Char
  lecHeadNames = compose (join . fmap maybeToList) lecNames (affineAsNat headAffine)
  uniHeadNames :: ITraversalAlg (HList '[String, String]) p (State Char) Char
  uniHeadNames = xcompose join uniLecturers lecHeadNames

-- XXX: requires undecidable and ambiguous instances
instance (FilterIndex (HList '[String]) p q (D q),
          At String p s (D q),
          UniversityData p q r s) => UniversityView p where
  uniName = runIdentity <$> view name
  departmentNames = fmap hHead <$> iindex departments
  totalBudget = getSum <$> ifold uniBudget (Sum . snd)
  getBudget s = listToMaybe <$> ifoci (uniOneBudget s)
  duplicateBudget = void $ imodi uniBudget (const (* 2))
  totalSalary = getSum <$> ifold uniSalary (Sum . snd)
  updateSalary f = void $ imodi uniSalary (const f)
  upperCase = void $ imodi uniHeadNames (const toUpper)
  nameInitials = ifoci uniHeadNames
  containsDepartment s = icontains departments (s `HCons` HNil)
  addDepartment i bdg lec =
    runIdentity <$> (mkDepartment bdg lec >>= set (at i) . Just)
  remDepartment i = runIdentity <$> set (at i) Nothing

instance FilterIndex (HList '[String]) (State University) (State Department) Department where
  filterIndex f sax =
    StateT (\u ->
      let (as, deps2) = unzip $ fmap (\(k2, d) -> fmap ((,) k2) (runState (sax (k2 `HCons` HNil)) d))
                                     (Prelude.filter (\(k2, _) -> f (k2 `HCons` HNil)) $ assocs $ deps u) in
      Identity (as, u { deps = deps u `difference` fromList deps2 `union` fromList deps2 }))

instance At String (State University) (State (Maybe Department)) Department where
  at i smd = StateT (\u -> let (x, a2) = runState smd (Data.Map.lookup i $ deps u) in
                           Identity (Identity x, u { deps = maybe (delete i (deps u)) (\v -> insert i v (deps u)) a2 }))

-- instantiating university

data University = University { nm :: String, deps :: Map String Department } deriving Show
data Department = Department { bud :: Int, lecs :: Map String Lecturer } deriving Show
data Lecturer = Lecturer { frt :: String, lst :: String, sly :: Int } deriving Show

-- XXX: use `lens` to generate this boilerplate

lensAsNat :: Lens s a -> (forall x. State a x -> State s (Identity x))
lensAsNat ln sax =
  StateT (\s -> let (out, a2) = runState sax (fst (runLens ln s)) in
                Identity (Identity out, snd (runLens ln s) a2))

universityDepartment :: String -> Lens University (Maybe Department)
universityDepartment i = Lens (\u -> (Data.Map.lookup i (deps u),
  \md -> u { deps = maybe (delete i (deps u)) (\d -> insert i d (deps u)) md }))

universityName :: Lens University String
universityName = Lens { runLens = \s -> (nm s, \a -> s { nm = a }) }

departmentBudget :: Lens Department Int
departmentBudget = Lens { runLens = \s -> (bud s, \a -> s { bud = a }) }

lecturerFirst, lecturerLast :: Lens Lecturer String
lecturerFirst = Lens { runLens = \s -> (frt s, \a -> s { frt = a }) }
lecturerLast = Lens { runLens = \s -> (lst s, \a -> s { lst = a }) }

lecturerSalary :: Lens Lecturer Int
lecturerSalary = Lens { runLens = \s -> (sly s, \a -> s { sly = a }) }

affineAsNat :: Affine s a -> AffineAlg (State s) (State a) a
affineAsNat af sax =
  StateT (\s -> case fmap (runState sax) (fst (runAffine af s)) of
    Just (x, a2) -> Identity (Just x, snd (runAffine af s) a2)
    Nothing -> Identity (Nothing, s))

headAffine :: Affine String Char
headAffine = Affine (\s -> case s of
  "" -> (Nothing, const "")
  (h : t) -> (Just h, \c -> c : t))

-- XXX: describe `TraversalAlg`s from `Traversal`, as we did with `Lens`
-- XXX: need to read Haskell style guide to split lines!
instance UniversityData (State University) (State Department) (State Lecturer) (State (Maybe Department)) where
  type D (State Department) = Department
  type L (State Lecturer) = Lecturer
  name = lensAsNat universityName
  departments sd =
    StateT (\u ->
      let (as, deps2) = unzip $ fmap (\(k, d) -> fmap ((,) k) (runState (sd (k `HCons` HNil)) d)) (assocs $ deps u) in
        Identity (as, u { deps = fromList deps2 }))
  department i = lensAsNat $ universityDepartment i
  budget = lensAsNat departmentBudget
  lecturers sl =
    StateT (\u ->
      let (as, lecs2) = unzip $ fmap (\(k, d) -> fmap ((,) k) (runState (sl (k `HCons` HNil)) d)) (assocs $ lecs u) in
        Identity (as, u { lecs = fromList lecs2 }))
  first = lensAsNat lecturerFirst
  last = lensAsNat lecturerLast
  salary = lensAsNat lecturerSalary
  mkDepartment bdg lec = fmap (Department bdg . fromList) plec where
    plec = traverse (\(f, l, s) -> fmap (fmap toLower (head f : l),) (mkLecturer (f, l, s))) lec
  mkLecturer (f, l, s) = return $ Lecturer f l s

-- XXX: replace `main` with tests ;)
main :: IO ()
main = do
  let jperez = Lecturer "jose" "perez" 3000
  let mlopez = Lecturer "maria" "lopez" 4000
  let cs = Department 10000 $ fromList [("jperez", jperez)]
  let hs = Department 20000 $ fromList [("mlopez", mlopez)]
  let uni = University "URJC" (fromList [("CS", cs), ("HS", hs)])
  putStr "* Total budget: "
  print $ evalState totalBudget uni
  putStr "* Budget (CS): "
  print $ evalState (getBudget "CS") uni
  putStr "* Budget (SP): "
  print $ evalState (getBudget "SP") uni
  putStr "* After duplicating budget: "
  print $ execState duplicateBudget uni
  putStr "* Department names: "
  print $ evalState departmentNames uni
  putStr "* Total salary: "
  print $ evalState totalSalary uni
  putStr "* After turning into upper case: "
  print $ execState upperCase uni
  putStr "* Getting name initials: "
  print $ evalState nameInitials uni
  putStr "* Contains department (CS): "
  print $ evalState (containsDepartment "CS") uni
  putStr "* Contains department (SP): "
  print $ evalState (containsDepartment "SP") uni
  putStr "* Remove Department (HS): "
  print $ execState (remDepartment "HS") uni
  putStr "* Add Department (Math): "
  print $ execState (addDepartment "Math" 4000 [("Eugenio", "Moggi", 1000)]) uni
