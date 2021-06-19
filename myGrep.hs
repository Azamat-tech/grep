import Data.List
import System.Exit
import Data.Maybe
import Algorithm.KMP
import System.Environment (getArgs)

type Indices        = [Int]
type NumText        = [EnumeratedLine]
type EnumeratedLine = (Int, String)

main :: IO ()
main = do  
    getArgs >>= parse

{-
    parses the input arguments
-}
parse :: [String] -> IO ()
parse ["--help"]    = usage >> exit 
parse ["--version"] = version >> exit
parse []            = exitArgumentMissing 
parse (word:file)   = start word file

exit :: IO a
exit = exitWith ExitSuccess

{-
    Returns the error message to the console
-}
exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = putStrLn str >> exitWith e

{-
    prepares the error messages and passes to the exitWithErrorMessage
-}
exitArgumentMissing :: IO a
exitArgumentMissing = 
    let argumentMissing = "Pattern is not provided. Please provide at least one word\n"
        info = "For additional information run : $ runghc myGrep --help"
    in exitWithErrorMessage (argumentMissing ++ info) (ExitFailure 2)


{-
    Returns the version of the program
-}
version :: IO ()
version = putStrLn "myGrep 1.0"

{-
    Returns the information on how to use the program. 
-}
usage :: IO ()
usage = 
    let userMsgFile = "usage example with text file: $ runghc myGrep.hs 'apple|orange' \"text.txt\" "
        userMsgTerminal = "usage example from standard input: $ runghc myGrep.hs 'apple|orange' "
    in putStrLn (userMsgFile ++ "\n" ++ userMsgTerminal)

{-
    Gets the word and file argumetns after parsing and starts the searching process.
-}
start :: String -> [String] -> IO ()
start wordsArg fileArg =
    -- listToMaybe returns (Just a) where a is the first element of the list, 
    -- or Nothing on an empty list.
    let listOfWords = splitWords wordsArg
    in chooseOption listOfWords $ listToMaybe fileArg

{-
    Splits the given input into the list of words. E.g 'apple|orange' -> ["apple","orange"]
-}
splitWords :: String -> [String]
splitWords = foldr (\c (x:xs) -> if c == '|' then []:x:xs else (c:x):xs) [[]]

{-
    Based on the argument read for the file the function chooses the suitable option.
    If the file is specified it read from file and continues the process. If not, 
    the function will read from the standard input and proceed.
-}
chooseOption :: [String] -> Maybe String -> IO ()   
chooseOption words (Just file) = do 
    content <- readFile file 
    putStr $ preSearch words content
chooseOption words Nothing = do
    content <- getContents
    putStr $ preSearch words content

{-
    This method prepares for the search by enumerating the content from the standard
    input or the file. 
-}
preSearch :: [String] -> String -> String
preSearch words content = 
    let splitLines = lines content
        numberedList = zip [1..] splitLines
    in matchLines words numberedList

{-
    Recursively passes the content of the file or the standad input to the search
    with the list of words. 
-}
matchLines :: [String] -> NumText -> String
matchLines words = concatMap (search words)

{-
    Given the list of words and the enumberated line, the search function 
    calls the KMP string matching algorithm to get the list of indicies where
    the word has appeared in the enumerated line. If it has, it output the
    result. Otherwise, it will check other words.
-}
search :: [String] -> EnumeratedLine -> String
search words numberedLine@(numberLine, line) = 
    let check :: String -> String
        check word = printLine numberedLine word result
            where result = kmpMatcher line word
    in concatMap (check) words

{-
    Prints the output of the search : the number of the line, the index position, 
    which word and the line itself.
-}
printLine :: EnumeratedLine -> String -> Indices -> String
printLine (number, line) word = 
    let prepareLine :: Int -> String 
        prepareLine x = 
            "(line " ++ show number ++ ") " 
            ++ "at index " ++ show x ++ " '" 
            ++ word ++ "'" ++ ": " ++ line 
            ++ "\n"
    in concatMap (prepareLine)