module Main where
import Test.QuickCheck

infixr 5 :>
data Stream a = a :> (Stream a)

streamToList :: Stream a -> [a]
streamToList = \(current :> rest) -> current : streamToList rest

instance Show a => Show (Stream a) where
  show = \stream -> init (show (take 20 (streamToList stream))) ++ ",..."

rep :: a -> Stream a
rep a = a :> rep a

-- | Example:
--
-- >>> rep 0
-- 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

-- | Example:
--
-- >>> from 5
-- 5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,...

nats :: Stream Integer
nats = from 0

-- | Example:
--
-- >>> nats
-- 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,...

zipStreamsWith :: (a->b->c) -> Stream a -> Stream b -> Stream c
zipStreamsWith f (x :> xs) (y :> ys) = (f x y) :> zipStreamsWith f xs ys

-- | Example:
--
-- >>> zipStreamsWith (+) nats nats
-- 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,...

instance Functor Stream where
  fmap = \fun (x :> xs) -> fun x :> fmap fun xs

-- | Example:
--
-- >>> fmap (+1) nats
-- 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...

instance Applicative Stream where
  pure = rep
  (<*>) = zipStreamsWith id
-- | Example:
--
-- >>> pure (+1) <*> nats
-- 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...

-- | Should satisfy:
--
--- >>> show (pure (*2) <*> nats) == show (fmap (*2) nats)
-- True

newtype Supply s a = S { runSupply :: Stream s -> (a, Stream s) }

get :: Supply s s
get = S (\(current :> rest) -> (current, rest))

-- | Example:
--
-- >>> evalSupply get nats
-- 0

pureSupply :: a -> Supply s a
pureSupply a = S (\stream -> (a, stream))

-- | Example:
--
-- >>> evalSupply (pure 42) nats
-- 42

mapSupply :: (a->b) -> Supply s a -> Supply s b
mapSupply f (S g) = S h where
  h s = let (a, gStream) = g s in 
        (f a, gStream)

-- | Example:
--
-- >>> evalSupply (mapSupply (+1) get) nats
-- 1

mapSupply2 :: (a->b->c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S ga) (S gb) = S gc where
  gc s = let (a, gaStream) = ga s in
         let (b, gbStream) = gb gaStream in
         (f a b, gbStream)

bindSupply :: Supply s a -> (a->Supply s b) -> Supply s b
bindSupply (S fa) k = S fb where
  fb s = let (a, faStream) = fa s in
         let (S fb) = k a in
         let (b, fbStream) = fb faStream in
         (b, fbStream)

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  (>>=) = bindSupply

evalSupply :: Supply s a -> Stream s -> a
evalSupply p s = fst $ runSupply p s

-- | Example:
--
-- >>> evalSupply (get >> get >> get) nats
-- 2

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving (Eq, Show)

size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = size l + size r

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch l r) = toList l ++ toList r

labelTree :: Tree a -> Tree Integer
labelTree t = evalSupply (go t) nats
  where
    go :: Tree a -> Supply s (Tree s)
    go = \tree -> case tree of
          (Branch treeL treeR) -> Branch <$> go treeL <*> go treeR
          (Leaf _)             -> Leaf <$> get
-- | Example:
--
-- >>> labelTree $ Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))
-- Branch (Leaf 0) (Branch (Leaf 1) (Leaf 2))


-- Hic sunt leones

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree'
    where tree' n | n <= 0 = Leaf <$> arbitrary
          tree' n =
                    oneof [Leaf <$> arbitrary,
                           Branch <$> subtree <*> subtree]
                    where subtree = tree' (n `div` 2)
  shrink (Leaf _) = []
  shrink (Branch l r) = [l, r] ++ [Branch l' r' | (l', r') <- shrink (l, r)]


prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = size (labelTree t) == size t

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == [0..n]
    where n = fromIntegral $ size t - 1

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = labelTree (labelTree t) == labelTree t

writeln = putStrLn

main = do
  writeln $ "-------------"
  writeln $ "-- STREAMS --"
  writeln $ "-------------"
  writeln $ ">>> rep 0 \n" ++ show (rep 0) ++ "\n"
  writeln $ ">>> from 5 \n" ++ show (from 5) ++ "\n"
  writeln $ ">>> nats \n" ++ show nats ++ "\n"
  writeln $ ">>> zipStreamsWith (+) nats nats \n" ++ 
              show (zipStreamsWith (+) nats nats) ++ "\n"
  writeln $ ">>> fmap (+1) nats \n" ++ show (fmap (+1) nats) ++ "\n"
  writeln $ ">>> pure (+1) <*> nats \n" ++ show (pure (+1) <*> nats) ++ "\n"
  writeln $ ">>> show (pure (*2) <*> nats) == show (fmap (*2) nats) \n" ++
              show (show (pure (*2) <*> nats) == show (fmap (*2) nats)) ++ "\n"
              
  writeln $ "--------------"
  writeln $ "-- SUPPLIES --"
  writeln $ "--------------"
  writeln $ ">>> evalSupply get nats \n" ++ show (evalSupply get nats) ++ "\n"
  writeln $ ">>> evalSupply (pure 42) nats \n" ++
              show (evalSupply (pure 42) nats) ++ "\n"
  writeln $ ">>> evalSupply (mapSupply (+1) get) nats \n" ++
              show (evalSupply (mapSupply (+1) get) nats) ++ "\n"
  writeln $ ">>> evalSupply (get >> get >> get) nats \n" ++
              show (evalSupply (get >> get >> get) nats) ++ "\n"
  
  writeln $ "-----------"
  writeln $ "-- TREES --"
  writeln $ "-----------"
  writeln $ ">>> labelTree $ Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c')) \n" ++
              show (labelTree $ Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))) ++ "\n"
  
  writeln $ "------------------"
  writeln $ "-- QUICK CHECKS --"
  writeln $ "------------------"
  writeln "prop_sizeLabelTree"
  quickCheck prop_sizeLabelTree
  writeln "prop_LabelTree"
  quickCheck prop_labelTree
  writeln "prop_LabelTreeIdempotent"
  quickCheck prop_labelTreeIdempotent
