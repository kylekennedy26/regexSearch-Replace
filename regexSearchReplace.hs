module FinalProj where

import Data.Char
import Data.List
--import Text.Regex.Posix


data Regex = Letter Char | Concat Regex Regex | Choice Regex Regex |
             Lambda | Empty | Star Regex

main :: IO
main = do
    --prompt user for input
    putStrLn "Welcome to RegEx Search and Replace. Enter a filename:"
    filename <- getLine
    putStrLn "Enter the string you want to find/match:"
    matchStr <- getLine
    putStrLn "Enter the string you want to replace the found string with:"
    replaceStr <- getLine
    --i only did this in 3 lines because idk how else to do it in haskell.
    fileContents <- readFile filename
    writeFile filename (runProgram fileContents matchStr replaceStr)
    putStrLn "Search and replace completed, file has been updated..."



validateInput :: String -> String -> String -> String
--x must be valid fileName, y must be valid string, z must be valid string.
validateInput x y z = error "not implemented yet"

match :: String -> Regex -> Bool
match (s:ss) (Letter x) = 