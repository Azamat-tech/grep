{-
KMP implementation that is translated from the pseudocode 
-}

module KMP2 where

kmp_search :: String -> String -> Bool
kmp_search pattern text = 
    let search indexT indexP = 
            | indexT == length text = False
            | otherwise = do
                | pattern !! indexP == text !! indexT = do
                    | (indexP + 1) == length pattern = True
                    | otherwise =  
          
    in search 0 0