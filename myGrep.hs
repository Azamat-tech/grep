import System.Environment (getArgs)
import Data.List
import Data.Maybe
import KMP

type EnumeratedLine = (Int, String)
type Table = [EnumeratedLine]

-- checks if the file is specified from the input or not. 
checkFile :: [String] -> Maybe String
checkFile (file : _) = Just file
checkFile _ = Nothing

-- Splits the given input into the list of words. E.g 'apple|orange' -> ["apple","orange"]
splitWords :: String -> [String]
splitWords = foldr (\c (x:xs) -> if c == '|' then []:x:xs else (c:x):xs) [[]]

-- display :: Table -> IO ()
-- display = mapM_ (\(a,b) -> putStrLn ("Line " ++ show a ++ ": " ++ b))

-- Prints the word that was found on the line given
printLine :: EnumeratedLine -> String -> IO () 
printLine (number, line) word = 
    putStrLn ("(line "++show number++") "++ "'"++word++"'"++": "++ line)

{-
This function takes the list of words and enumerated line. It calls the KMP
algorithm to find if the words are in the line.
-}
search :: [String] -> EnumeratedLine -> IO ()
search [] _ = return ()
search (word : otherWords) numberedLine@(numberLine, line) = do
    if isSubstringOf word line then do 
            printLine numberedLine word
            search otherWords numberedLine
        else 
            search otherWords numberedLine

-- passes the text to the search method line by line with list of words
checkLines :: [String] -> Table -> IO ()
checkLines _ [] = return ()
checkLines words (line : numberedList) = do
    search words line
    checkLines words numberedList 

-- enumerates the lines
preSearch :: [String] -> String -> IO ()
preSearch words content = do
    let splitLines = lines content
        enumaratedList = [ (a, b) | (a, b) <- zip [1..] splitLines ]
    checkLines words enumaratedList

-- decides if the function should read from the standard input or from the file 
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