import Data.List
import System.Exit
import Data.Maybe
import Algorithm.KMP
import Control.Monad
import System.Environment (getArgs)

type Indices        = [Int]
type NumText        = [EnumeratedLine]
type EnumeratedLine = (Int, String)

-- checks if the file is specified from the input or not. 
checkFile :: [String] -> Maybe String
checkFile (file : _) = Just file
checkFile _ = Nothing

-- Splits the given input into the list of words. E.g 'apple|orange' -> ["apple","orange"]
splitWords :: String -> [String]
splitWords = foldr (\c (x:xs) -> if c == '|' then []:x:xs else (c:x):xs) [[]]

-- Prints the word that was found on the line given
printLine :: EnumeratedLine -> String -> Indices -> IO () 
printLine (number, line) word indices = 
    let loop :: Indices -> IO ()
        loop [] = return ()
        loop (x : xs) = 
            putStrLn 
                ( 
                    "(line " ++ show number ++ ") " ++ "at index " ++ show x ++ " '" ++ word ++ "'" ++ ": " ++ line 
                )
    in loop indices

nonEmpty :: Indices -> Bool 
nonEmpty list 
    | length list /= 0 = True
    | otherwise = False

{-
This function takes the list of words and enumerated line. It calls the KMP
algorithm to find if the words are in the line.
-}
search :: [String] -> EnumeratedLine -> IO ()
search [] _ = return ()
search (word : otherWords) numberedLine@(numberLine, line) = do
    let result = kmpMatcher line word
    if nonEmpty result then do
        printLine numberedLine word result
        search otherWords numberedLine
    else 
        search otherWords numberedLine

-- passes the text to the search method line by line with list of words
checkLines :: [String] -> NumText -> IO ()
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

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = putStrLn str >> exitWith e

exitArgumentMissing :: IO a
exitArgumentMissing = 
    let argumentMissing = "Pattern is not provided. Please provide at least one word\n"
        info = "For additional information run : $ runghc myGrep --help"
    in exitWithErrorMessage (argumentMissing ++ info) (ExitFailure 2)

isWord :: String -> Bool 
isWord (x:input) = if x == '\'' then True else False

exit :: IO a
exit    = exitWith ExitSuccess

parse :: [String] -> IO ()
parse ["--help"] = usage >> exit 
parse [] = exitArgumentMissing 
parse (x:input) 
    | isWord x = start x input 
    | otherwise = putStrLn ((show x) ++ "HERE")

usage :: IO ()
usage = 
    let userMsgFile = "usage example with text file: $ runghc myGrep.hs 'apple|orange' \"text.txt\" "
        userMsgTerminal = "usage example from standard input: $ runghc myGrep.hs 'apple|orange' "
    in putStrLn (userMsgFile ++ "\n" ++ userMsgTerminal)

start :: String -> [String] -> IO ()
start wordsArg fileArg =
    let file = checkFile fileArg
        listOfWords = splitWords wordsArg
    in chooseOption listOfWords file

tac  = unlines . reverse . lines

main :: IO ()
main = do  
    getArgs >>= parse