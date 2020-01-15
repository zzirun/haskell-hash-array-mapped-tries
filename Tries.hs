module Tries where

import Data.List hiding (insert)
import Data.Bits
import Data.Char

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes num
  = sum [mod (ord b - indexZero) 2 | b <- bs]
  where
    bs = showBitVector num maxBits
    maxBits = 64
    indexZero = 48
    

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = countOnes maskedN
  where
    maskedN = n .&. (bit i - 1)

bitToInt :: String -> Int
bitToInt "0" = 0
bitToInt "1" = 1
bitToInt bs
  = (bitToInt b') * 2 + (bitToInt b)
  where
    b' = take (length bs - 1) bs
    b = drop (length bs - 1) bs

getIndex :: Int -> Int -> Int -> Int
getIndex num bNum bSize
  = shiftedNum .&. (bit bSize - 1)
  where
    shiftedNum = shiftR num (bNum * bSize)

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace _ [] v = [v]
replace n (l : ls) v
  | n == 0 = (v : ls)
  | otherwise = l : (replace (n - 1) ls v)

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt _ v [] = [v]
insertAt n v (l : ls)
  | n == length (l : ls) = (l : ls) ++ [v]
  | n == 0 = v : (l : ls)
  | otherwise = l : (insertAt (n - 1) v ls)

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f f' (Leaf xs)
  = f' xs
sumTrie f f' (Node x sns)
  = sum [sumTrie' f f' sn | sn <- sns]
  where
    sumTrie' :: (Int -> Int) -> ([Int] -> Int) -> SubNode -> Int
    sumTrie' f f' (Term x)
      = f x
    sumTrie' f f' (SubTrie t)
      = sumTrie f f' t


trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member v hash trie y
  = member' v hash trie y 0
  where
    member' :: Int -> Hash -> Trie -> Int -> Int -> Bool
    member' v h (Node bv sns) y level
      | i' = member'' v (sns !! n)
      | otherwise = False 
      where
        i = getIndex h level y
        i' = testBit bv i
        n = countOnesFrom i bv
        member'' :: Int -> SubNode -> Bool
        member'' v (Term x)
          = v == x
        member'' v (SubTrie t)
          = member' v hash t y (level + 1)
    member' v h (Leaf xs) n level
      = elem v xs


--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hashF maxD bSize v t
  = insert' hashF maxD bSize v t 0
  where
    insert' :: HashFun -> Int -> Int -> Int -> Trie -> Int -> Trie
    insert' hashF maxD bSize v t level
      | level == maxD - 1 = Leaf [v]
    insert' hashF maxD bSize v (Leaf vs) level
      | elem v vs = Leaf vs
      | otherwise = Leaf (v : vs)
    insert' hashF maxD bSize v (Node bv sns) level
      | i' = Node bv (replace n sns (insert'' v (sns !! n)))
      | otherwise = Node (setBit bv i) (insertAt 0 (Term v) sns) 
      where
        i = getIndex (hashF v) level 4
        i' = testBit bv i
        n = countOnesFrom i bv
        insert'' :: Int -> SubNode -> SubNode
        insert'' v (SubTrie t)
          = SubTrie (insert hashF maxD bSize v t)
        insert'' v (Term v')
          | v == v' = Term v'
          | otherwise = SubTrie insertedV
          where
            insertedV' = insert hashF maxD bSize (hashF v') empty
            insertedV = insert hashF maxD bSize (hashF v) insertedV'

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie
  = undefined
