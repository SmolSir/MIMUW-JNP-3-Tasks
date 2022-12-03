{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = program

program :: Program
program = putStr(runTests)

elemList :: Eq a => a -> [a] -> Bool
elemList value list = foldList (\val acc -> if value == val then True else acc) False list

appendList :: [a] -> [a] -> [a]
appendList prior latter = foldList (\val acc -> val : acc) latter prior 

listLength :: [a] -> Integer
listLength list = foldList (\_ acc -> acc + 1) 0 list

filterList :: (a -> Bool) -> [a] -> [a]
filterList function list = foldList (\val acc -> if function val then val : acc else acc) [] list

-- if position > list length returns last value
nth :: [a] -> Integer -> a
nth list position = 
  let pos = min (listLength list) position in
  foldList (\(idx, val) acc -> if idx == pos then val else acc) (head list) (zip [1..] list)

mapList :: (a -> b) -> [a] -> [b]
mapList function list = foldList (\val acc -> function val : acc) [] list

-- for an empty list, assume True (none of the entries was False)
andList :: [Bool] -> Bool
andList list = foldList (\val acc -> val && acc) True list

-- for an empty list, assume True (none of the entries can be evaluated to False)
allList :: (a -> Bool) -> [a] -> Bool
allList function list = foldList (\val acc -> function val && acc) True list

-- folds from the right to the left
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList function initialValue []   = initialValue
foldList function initialValue (h:t) = function h (foldList function initialValue t)

runTests :: String
runTests = foldList (\result acc -> result ++ acc) "" tests

tests :: [String]
tests = [
  if elemList 1 [] == False then "elemList 1 OK\n" else "elemList 1 WRONG\n",
  if elemList 0 [1, 2, 3, 4, 5] == False then "elemList 2 OK\n" else "elemList 2 WRONG\n",
  if elemList 4 [1, 2, 3, 4, 5] == True then "elemList 3 OK\n" else "elemList 3 WRONG\n",
  "\n",
  if appendList [1, 2, 3] [4, 5, 6] == [1, 2, 3, 4, 5, 6] then "appendList 1 OK\n" else "appendList 1 WRONG\n",
  if appendList [] [1, 2, 3] == [1, 2, 3] then "appendList 2 OK\n" else "appendList 2 WRONG\n",
  if appendList [1, 2, 3] [] == [1, 2, 3] then "appendList 3 OK\n" else "appendList 3 WRONG\n",
  "\n",
  if listLength [] == 0 then "listLength 1 OK\n" else "listLength 1 WRONG\n",
  if listLength [1, 2, 3] == 3 then "listLength 2 OK\n" else "listLength 2 WRONG\n",
  "\n",
  if filterList (\val -> even val) [1, 2, 3, 4, 5] == [2, 4] then "filterList 1 OK\n" else "filterList 1 WRONG\n",
  if filterList (\val -> False) [1, 2, 3, 4, 5] == [] then "filterList 2 OK\n" else "filterList 2 WRONG\n",
  "\n",
  if nth [1, 2, 3, 4, 5] 2 == 2 then "nth 1 OK\n" else "nth 1 WRONG\n",
  if nth [1, 2, 3, 4, 5] 10 == 5 then "nth 2 OK\n" else "nth 2 WRONG\n",
  "\n",
  if mapList (\val -> val * 2) [1, 2, 3] == [2, 4, 6] then "mapList 1 OK\n" else "mapList 1 WRONG\n",
  if mapList (\val -> val ++ val) ["a", "b", "c"] == ["aa", "bb", "cc"] then "mapList 2 OK\n" else "mapList 2 WRONG\n",
  "\n",
  if andList [True, True] == True then "andList 1 OK\n" else "andList 1 WRONG\n",
  if andList [True, False] == False then "andList 2 OK\n" else "andList 2 WRONG\n",
  if andList [] == True then "andList 3 OK\n" else "andList 3 WRONG\n",
  "\n",
  if allList (\val -> even val) [2, 4, 6] == True then "allList 1 OK\n" else "allList 1 WRONG\n",
  if allList (\val -> odd val) [1, 2, 3] == False then "allList 2 OK\n" else "allList 2 WRONG\n",
  if allList (\val -> False) [] == True then "allList 3 OK\n" else "allList 3 WRONG\n",
  "\n"
  ]