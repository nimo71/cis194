{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.List
import Data.Monoid
import Data.Tree

---------------------------------------------------------------
-- Exercise 1
---------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + (empFun e))

instance Monoid GuestList where
  mempty = GL [] 0
  (GL es1 fun1) `mappend` (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if (gl1 > gl2) then gl1 else gl2


---------------------------------------------------------------
-- Exercise 2
---------------------------------------------------------------

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z (Node tag children) = f tag $ map (treeFold f z) children


---------------------------------------------------------------
-- Exercise 3
---------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (newWithBoss, newWithoutBoss)
  where prevWithBoss = map fst bestLists
        prevWithoutBoss = map (uncurry moreFun) bestLists
        newWithBoss = glCons boss $ mconcat prevWithoutBoss
        newWithoutBoss = mconcat prevWithBoss


        
---------------------------------------------------------------
-- Exercise 4
---------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel mempty


---------------------------------------------------------------
-- Exercise 5
---------------------------------------------------------------

totalFun :: GuestList -> Fun
totalFun (GL _ fun) = fun

sortGuests :: GuestList -> [Name]
sortGuests (GL employees _) = sort $ map empName employees 

formatOutput :: Tree Employee -> String
formatOutput employees = "Total fun: " ++ show fun ++ "\n" ++ sortedGuests
  where guestList = maxFun employees
        fun = totalFun guestList
        sortedGuests = unlines $ sortGuests guestList

main :: IO ()
main = readFile "company.txt" >>= putStrLn . formatOutput . read
