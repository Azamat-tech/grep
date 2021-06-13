import System.Environment (getArgs)
import Data.List
import Data.Maybe
import KMP1

type EnumeratedLine = (Int, String)
type Table = [EnumeratedLine]

checkFile :: [String] -> Maybe String
checkFile (file : _) = Just file
checkFile _ = Nothing

splitWords :: String -> [String]
splitWords = foldr (\c (x:xs) -> if c == '|' then []:x:xs else (c:x):xs) [[]]

display :: Table -> IO ()
display = mapM_ (\(a,b) -> putStrLn ("Line " ++ show a ++ ": " ++ b))

printLine :: EnumeratedLine -> String -> IO () 
printLine (number, line) word = 
    putStrLn ("(line "++show number++") "++ "'"++word++"'"++": "++ line)

search :: [String] -> EnumeratedLine -> IO ()
search [] _ = return ()
search (word : otherWords) numberedLine@(numberLine, line) = do
    if isSubstringOf word line then do 
            printLine numberedLine word
            search otherWords numberedLine
        else 
            search otherWords numberedLine

checkLines :: [String] -> Table -> IO ()
checkLines _ [] = return ()
checkLines words (line : numberedList) = do
    search words line
    checkLines words numberedList 

preSearch :: [String] -> String -> IO ()
preSearch words content = do
    let stplitLines = lines content
        enumaratedList = [ (a, b) | (a, b) <- zip [1..] stplitLines ]
    checkLines words enumaratedList


chooseOption :: [String] -> Maybe String -> IO ()
chooseOption words (Just file) = do 
    content <- readFile file 
    preSearch words content
chooseOption words Nothing = do
    content <- getContents
    preSearch words content

main :: IO ()
main = do  
    (wordsArg : fileArg) <- getArgs 

    let file = checkFile fileArg
        listOfWords = splitWords wordsArg

    chooseOption listOfWords file