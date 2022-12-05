module FinalProj where

import Data.Char
import Data.List

data Regex = Letter Char | Concat Regex Regex | Choice Regex Regex |
             Lambda | Empty | Star Regex

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


--the types below are placeholder and not correct i think

runner :: String -> String -> String -> String
runner file matchStr replaceStr = error "not implemented"

search :: String -> String
search x = error "not implemented"

replace :: String -> String
replace x = error "not implemented"

lexer :: String -> [Regex]
lexer x = error "not implemented"

parser :: [Regex] -> String
parser x = error "not implemented"

sr :: [Regex] -> [Regex] -> [Regex]
sr x y = error "not implemented"
