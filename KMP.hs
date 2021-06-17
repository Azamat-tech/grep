module KMP where

import Data.Array 

data Table a = Table { wordTable :: Array Int a, jumpTable :: Array Int Int }

toArray :: [a] -> Int -> Array Int a
toArray pattern size = listArray (0, size - 1) pattern

computePrefixFunction :: Array Int a -> Int -> Array Int Int 
computePrefixFunction wordTable size = 
    let index         = 1
        longestPrefix = 0
        prefixTable = toArray (take size $ repeat 0) size
        construct prefixTable index longestPrefix 
            | index >= size = prefixTable
            | otherwise = 
                if wordTable ! index == wordTable ! longestPrefix then
                    construct (prefixTable // [(index, longestPrefix + 1)]) (index + 1) (longestPrefix + 1)
                    else if longestPrefix > 0 then
                        construct prefixTable index (prefixTable ! longestPrefix)
                    else construct (prefixTable // [(index, 0)]) (index + 1) (longestPrefix)
    
    in construct prefixTable index longestPrefix

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
        prefixList = computePrefixFunction patternArr patternSize

        indexP = 0 -- index for patternArr
        indexT = 0 -- index for text

        match indexP indexT 
            | indexT >= textSize = []
            | otherwise = 
                if patternArr ! indexP == textArr ! indexT then
                    if (indexP + 1) == patternSize then 
                        (indexT - indexP) : match (prefixList ! indexP) (index + 1)
                    else match (indexP + 1) (indexT + 1)
                else if indexT < textSize && patternArr ! indexP /= textArr ! indexT then
                    if indexP /= 0 
                        then match (prefixList ! (indexP - 1)) indexT
                        else match indexP (indexT + 1)
                else []
    in match indexP indexT


{-
https://www.geeksforgeeks.org/kmp-algorithm-for-pattern-searching/ - GeeksForGeeks

def KMPSearch(pat, txt):
    M = len(pat)
    N = len(txt)

    lps = [0]*M
    j = 0 # index for pat[]

    computeLPSArray(pat, M, lps)
  
    i = 0 # index for txt[]
    while i < N:
        if pat[j] == txt[i]:
            i += 1
            j += 1
  
        if j == M:
            print ("Found pattern at index " + str(i-j))
            j = lps[j-1]

        elif i < N and pat[j] != txt[i]:
            if j != 0:
                j = lps[j-1]
            else:
                i += 1
-}