{-# LANGUAGE RankNTypes #-}

module UniversityLens where

import Control.Lens

import Data.Char
import Data.Map

-- university data structures

data University = University { nm :: String
                             , deps :: Map String Department } deriving Show

data Department = Department { bud :: Int
                             , lecs :: Map String Lecturer } deriving Show

data Lecturer = Lecturer { frt :: String
                         , lst :: String
                         , sly :: Int } deriving Show

-- optics

-- XXX: these lenses could be generated automatically, they are provided for
-- pedagogical reasons

name :: Lens' University String
name = lens nm (\u nm2 -> u { nm = nm2 })

departments :: Lens' University (Map String Department)
departments = lens deps (\u deps2 -> u { deps = deps2 })

budget :: Lens' Department Int
budget = lens bud (\d bud2 -> d { bud = bud2 })

lecturers :: Lens' Department (Map String Lecturer)
lecturers = lens lecs (\d lecs2 -> d { lecs = lecs2 })

first :: Lens' Lecturer String
first = lens frt (\l frt2 -> l { frt = frt2 })

last :: Lens' Lecturer String
last = lens lst (\l lst2 -> l { lst = lst2 })

salary :: Lens' Lecturer Int
salary = lens sly (\l sly2 -> l { sly = sly2 })

firstLast :: Traversal' Lecturer String
firstLast f l = (\s1 s2 -> l { frt = s1, lst = s2 }) <$> f (frt l) <*> f (lst l)

-- logic

uniName :: University -> String
uniName = view name

departmentNames :: University -> [String]
departmentNames = keys . view departments

totalBudget :: University -> Int
totalBudget u = sum $ u ^.. (departments . each . budget)

getBudget :: String -> University -> Maybe Int
getBudget s u = u ^? (departments . at s . _Just . budget)

duplicateBudget :: University -> University
duplicateBudget = over (departments . each . budget) (*2)

totalSalary :: University -> Int
totalSalary u = sum $ u ^.. (departments . each . lecturers . each . salary)

updateSalary :: (Int -> Int) -> University -> University
updateSalary = over (departments . each . lecturers . each . salary)

upperCase :: University -> University
upperCase = over tr (fmap toUpper) where
  tr = departments . each . lecturers . each . firstLast

nameInitials :: University -> String
nameInitials u = u ^.. tr where
  tr = departments . each . lecturers . each . firstLast . _head

containsDepartment :: String -> University -> Bool
containsDepartment s u = member s (view departments u)

addDepartment :: String -> Int -> [(String, String, Int)] -> University -> University
addDepartment s i xs u = u & (departments . at s) .~ Just dep where
  dep = Department i lec
  lec = fromList (fmap (\(fr, ls, sl) -> (head fr : ls, Lecturer fr ls sl)) xs)

remDepartment :: String -> University -> University
remDepartment s u = u & (departments . at s) .~ Nothing
