module KMP where

import Data.Array 

type IndexElement = (Int, Int)
type IntArray = Array Int Int

data Table a = Table { wordTable :: Array Int a, prefixTable :: IntArray } deriving (Show)

toArray :: [a] -> Int -> Array Int a
toArray pattern size = listArray (0, size - 1) pattern

changeElement :: IntArray -> IndexElement -> IntArray
changeElement prevArray (index, element) = prevArray // [(index, element)]

computePrefixFunction :: Eq a => [a] -> Int -> Table a
computePrefixFunction pattern size = 
    let index         = 1
        longestPrefix = 0
        table = Table 
            { 
                wordTable = toArray pattern size, 
                prefixTable = construct (toArray (take size $ repeat 0) size) index longestPrefix
            }
        construct prefixTable index longestPrefix 
            | index >= size = prefixTable
            | otherwise = 
                if (wordTable table) ! index == (wordTable table) ! longestPrefix then
                    construct (changeElement prefixTable (index, longestPrefix + 1)) (index + 1) (longestPrefix + 1)
                    else if longestPrefix > 0 then
                        construct prefixTable index (prefixTable ! longestPrefix)
                    else construct (changeElement prefixTable (index, 0)) (index + 1) (longestPrefix)
    in table
{-
The function takes the text and the pattern as arguments and returns 
the list of indices where the pattern appeared in the
-}
kmpMatcher :: Eq a => [a] -> [a] -> [Int]
kmpMatcher text pattern = 
    let textSize    = length text
        textArr     = toArray text textSize
        patternSize = length pattern
        patternArr  = toArray pattern patternSize
        
        -- preprocessed pattern
        table = computePrefixFunction pattern patternSize

        indexP = 0 -- index for patternArr
        indexT = 0 -- index for text

        match :: Int -> Int -> [Int]
        match indexP indexT 
            | indexT >= textSize = []
            | otherwise = 
                if patternArr ! indexP == textArr ! indexT then
                    if (indexP + 1) == patternSize then 
                        (indexT - indexP) : match ((prefixTable table) ! indexP) (indexT + 1)
                    else match (indexP + 1) (indexT + 1)
                else if indexT < textSize && patternArr ! indexP /= textArr ! indexT then
                    if indexP /= 0 
                        then match ((prefixTable table) ! (indexP - 1)) indexT
                        else match indexP (indexT + 1)
                else []
    in match indexP indexT