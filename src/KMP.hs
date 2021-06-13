{- 
KMP data type: Either the entire needle is matched to a text
or we  move on at a new position at the table that we construct
for the needle.

IsSubstringOf return the True if the pattern is in the text and 
False if not.

The source for implementation of KMP -> 
    https://twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell
-}

module KMP where

data KMP a = KMP  { found :: Bool, next :: (a -> KMP a) }

-- The matching process 
isSubstringOf :: Eq a => [a] -> [a] -> Bool
isSubstringOf as bs = match (build as) bs
   where  match table []     = found table
          match table (b:bs) = found table || match (next table b) bs

-- The build function get the pattern and generates the KMP table (automata)
build :: Eq a => [a] -> KMP a 
build pattern = table 
    where table = buildHelper pattern (const table)

buildHelper :: Eq a => [a] -> (a -> KMP a) -> KMP a 
buildHelper [] failure = KMP True failure
buildHelper (x : xs) failure = KMP False test 
    where test char = if char == x then success else failure char
          success = buildHelper xs (next (failure x))
