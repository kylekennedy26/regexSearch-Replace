module FinalProj where
import Data.Char
import Data.List

data Regex = Letter Char | Concat Regex Regex | Choice Regex Regex |
             Lambda | Empty | Star Regex
    deriving(Show,Eq)

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
    writeFile filename (runProgram fileContents matchStr replaceStr)
    putStrLn ("Sucessfully replaced " ++ matchStr ++ " with " ++ replaceStr ++ " in " ++ filename)

match :: Regex -> String -> Bool
match Empty st = st == ""
match Lambda st = st == "\\" --is this the right character for lambda ?
match (Letter ch) [] = False
match (Letter ch) (firstChar:rst) = firstChar == ch
match (Choice r1 r2) st = match r1 st || match r2 st
match (Concat r1 r2) st = or [match r1 s1 && match r2 s2 | (s1, s2)<- splitString st]
match (Star r) st = match Empty st || or [match r s1 && match (Star r) s2 | (s1, s2) <- frontSplit st]

splitString :: [a] -> [([a], [a])]
splitString str = [ splitAt n str | n <- [0 .. length str] ]

--frontSplit is the same as splitString, but it excludes the split ([], str)
frontSplit :: [a] -> [([a], [a])]
frontSplit str = [splitAt n str | n <- [1 .. length str]]


matchPrefix :: Regex -> [Char] -> [String]
matchPrefix Empty str = [""] --changed this from []
matchPrefix Lambda str = [str]
matchPrefix (Letter ch) [] = [] --changed this from [""]
matchPrefix (Letter ch) (strChar:rst) = [rst | ch == strChar]
matchPrefix (Concat r1 r2) str = [ s3 | (s1, s2, s3) <- splitThree str, match r1 s1 && match r2 s2 ]
matchPrefix (Star r) str = matchPrefix (Choice Lambda (Concat r (Star r))) str
--choice?

--helper functions for match prefix
splitThree :: [a] -> [([a], [a], [a])]
--generate all the possible tuples for s1 ++ s2 ++ s3 = str.
--Repeated characters are treated as different. (Hello generates Hel twice since two ls.)
splitThree str = [zip3 xs ys zs | (xs, ys) <- unzip (splitString str), zs <- unzip (splitString ys)]

--given 3 lists, zip them together into a list of triples.
zip3' :: [a] -> [a] -> [a] -> [([a], [a], [a])]
zip3' xs ys [] = []
zip3' xs [] zs = []
zip3' [] ys zs = []
zip3' (x:xs) (y:ys) (z:zs) = ([x], [y], [z]) : zip3 [xs] [ys] [zs] 

runProgram :: String -> String -> String -> String
--takes as input 2 strings, and a string representing the filecontents as 1 huge string.
runProgram file matchStr replaceStr = error "not implemented"

search :: String -> Regex -> Bool
search x = error "not implemented"

replace :: String -> String
replace x = error "not implemented"
