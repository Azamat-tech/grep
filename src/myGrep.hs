import System.Environment (getArgs)
import Data.List
import Data.Maybe

checkFile :: [String] -> Maybe String
checkFile (file : _) = Just file
checkFile _ = Nothing

splitWords :: String -> [String]
splitWords = foldr (\c (x:xs) -> if c == '|' then []:x:xs else (c:x):xs) [[]]

search :: [String] -> String -> IO [String]
search ...

chooseOption :: [String] -> Maybe String -> IO ()
chooseOption words (Just file) = search words file
chooseOption words Nothing = 
    let content = getContents
    in search words content 

main :: IO [()]
main = do  
    (wordsArg : fileArg) <- getArgs 

    let file = checkFile fileArg
    let listOfWords = splitWords wordsArg

    chooseOption listOfWords file
