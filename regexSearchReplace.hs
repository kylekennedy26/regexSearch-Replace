module FinalProj where
import Data.Char
import Data.List

data Regex = Letter Char | Concat Regex Regex | Choice Regex Regex |
             Lambda | Empty | Star Regex
    deriving(Show,Eq)

--choice  -> https://chortle.ccsu.edu/finiteautomata/Section07/sect07_7.html

main :: IO ()
main = do
    --prompt user for input
    putStrLn "Welcome to RegEx Search and Replace. Enter a filename:"
    filename <- getLine
    putStrLn "Enter the string you want to find/match:"
    matchStr <- getLine
    putStrLn "Enter the string you want to replace the found string with:"
    replaceStr <- getLine
    --read file
    fileContents <- readFile filename
    --do stuff
    writeFile filename (runner fileContents matchStr replaceStr)
    putStrLn ("Sucessfully replaced " ++ matchStr ++ " with " ++ replaceStr ++ " in " ++ filename)

--do we need to pattern match for lambda here?
match :: Regex -> String -> Bool
match Empty st = st == ""
match (Letter ch) st = st == [ch]
match (Choice r1 r2) st = match r1 st || match r2 st
match (Concat r1 r2) st = or [match r1 s1 && match r2 s2 | (s1, s2)<- splitString st]
match (Star r) st = match Empty st || or [match r s1 && match (Star r) s2 | (s1, s2) <- frontSplit st]

splitString :: [a] -> [([a], [a])]
splitString str = [ splitAt n str | n <- [0 .. length str] ]

--frontSplit is the same as Split, but it excludes the split ([], str)
frontSplit :: [a] -> [([a], [a])]
frontSplit str = [splitAt n str | n <- [1 .. length str]]

matchPrefix :: Regex -> String -> [String]
--USE RECURSION. this function checks whether a prefix of a string can be matched, 
--and returns a list of strings that would remain, for every possible match of the input string.
matchPrefix Empty st = st == []
matchPrefix Lambda st = st == [""]

runProgram :: String -> String -> String -> String
--takes as input 2 strings, and a string representing the filecontents as 1 huge string.
--do we need to run words on it to make it a list of strings?
runProgram file matchStr replaceStr = error "not implemented"

search :: String -> Regex -> Bool
search x = error "not implemented"

replace :: String -> String
replace x = error "not implemented"
