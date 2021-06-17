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
    let file = checkFile fileArg
        listOfWords = splitWords wordsArg
    in chooseOption listOfWords file

{-
    Checks if the function is specified from the command line. Returns Just file if specified.
    Otherwise, returns Nothing
-}
checkFile :: [String] -> Maybe String
checkFile (file : _) = Just file
checkFile _ = Nothing

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
    preSearch words content
chooseOption words Nothing = do
    content <- getContents
    preSearch words content

{-
    This method prepares for the search by enumerating the content from the standard
    input or the file. 
-}
preSearch :: [String] -> String -> IO ()
preSearch words content = do
    let splitLines = lines content
        enumaratedList = [ (a, b) | (a, b) <- zip [1..] splitLines ]
    mathcLines words enumaratedList

{-
    Recursively passes the content of the file or the standad input to the search
    with the list of words. 
-}
mathcLines :: [String] -> NumText -> IO ()
mathcLines _ [] = return ()
mathcLines words (line : numberedList) = do
    search words line
    mathcLines words numberedList 

{-
    Given the list of words and the enumberated line, the search function 
    calls the KMP string matching algorithm to get the list of indicies where
    the word has appeared in the enumerated line. If it has, it output the
    result. Otherwise, it will check other words.
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

{-
    Checks if the list of indicies is empty or not.
-}
nonEmpty :: Indices -> Bool 
nonEmpty list 
    | length list /= 0 = True
    | otherwise = False

{-
    Prints the output of the search : the number of the line, the index position, 
    which word and the line itself.
-}
printLine :: EnumeratedLine -> String -> Indices -> IO () 
printLine (number, line) word indices = 
    let loop :: Indices -> IO ()
        loop [] = return ()
        loop (x : xs) = do
            putStrLn ("(line " ++ show number ++ ") " 
                        ++ "at index " ++ show x ++ " '" 
                        ++ word ++ "'" ++ ": " ++ line )
            loop xs
    in loop indices