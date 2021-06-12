import System.Environment (getArgs)
import Data.List
import Data.Maybe

type EnumeratedLine = (Int, String)
type Table = [EnumeratedLine]

checkFile :: [String] -> Maybe String
checkFile (file : _) = Just file
checkFile _ = Nothing

splitWords :: String -> [String]
splitWords = foldr (\c (x:xs) -> if c == '|' then []:x:xs else (c:x):xs) [[]]

display :: Table -> IO ()
display = mapM_ (\(a,b) -> putStrLn ("Line " ++ show a ++ ": " ++ b))

-- NOT KMP ALG. This version just looks over the prefixes. More like using isPrefixOf function
-- to get the result. This version will be updated 
kmpSearch :: String -> String -> Bool
kmpSearch pattern text 
    | length pattern > length text = False
    | pattern == (take (length pattern) text) = True
    | pattern /= (take (length pattern) text) = (kmpSearch pattern (tail text))

search :: [String] -> EnumeratedLine -> IO ()
search [] (numberLine, line) = return ()
search (word : otherWords) (numberLine, line)= do
    let strings = lines line 
    let result = filter(\x -> kmpSearch word x) strings 
    if length result == 0 then search otherWords (numberLine, line) 
        else putStrLn ("Line " ++ show numberLine ++ "(" ++ word ++ ")" ++ ": " ++ line)

checkLines :: [String] -> Table -> IO ()
checkLines _ [] = return ()
checkLines words (line : numberedList) = do
    search words line
    checkLines words numberedList 

preSearch :: [String] -> String -> IO ()
preSearch words content = do
    let stplitLines = lines content
    let enumaratedList = [(a, b) | (a, b) <- zip [1..] stplitLines]
    checkLines words enumaratedList
        
    -- display enumaratedList
    -- putStrLn "###"
    -- let result = kmpSearch x content 
    -- putStrLn (show (length (lines content)))
    -- mapM putStrLn words 

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
    let listOfWords = splitWords wordsArg

    chooseOption listOfWords file