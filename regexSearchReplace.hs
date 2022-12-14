module FinalProj where
import Data.Char
import Data.List

data Regex = Letter Char | Concat Regex Regex | Choice Regex Regex |
             Lambda | Empty | Star Regex
    deriving(Show,Eq)

main :: IO ()
main = do
    --prompt user for input
    putStrLn "To perform Regex Search & Replace,\nEnter a filename:"
    filename <- getLine
    putStrLn "Enter the string you want to find/match:"
    matchStr <- getLine
    putStrLn "Enter the string you want to replace the found string with:"
    replaceStr <- getLine
    --read file
    fileContents <- readFile filename
    --if fileContents are the same then say so
    if runProgram fileContents matchStr replaceStr == fileContents then putStrLn "No matches found to replace." else putStrLn ("Sucessfully replaced " ++ matchStr ++ " with " ++ replaceStr ++ " in " ++ filename)
    writeFile filename (runProgram fileContents matchStr replaceStr)
    --loop
    putStrLn "Would you like to perform another Regex Search & Replace? (y/n)"
    q <- getLine
    if q == "yes" || q == "y" then main else putStrLn "\n"


--takes as input 3 strings, and a string representing the filecontents as 1 huge string.
--string to match, turned into a regex pattern
--string that will replace
runProgram :: String -> String -> String -> String
runProgram file matchStr replaceStr = if search x file then replace x file replaceStr else file
                                        where x = genPattern (intersperse ';' matchStr)

search :: Regex -> String -> Bool
search reg file = not (null (matchPrefix reg file))

replace :: Regex -> String -> String -> String
replace r file str = str ++ head (matchPrefix r file)

genPattern :: String -> Regex
genPattern [] = Empty
genPattern s = parser s

match :: Regex -> String -> Bool
match Empty st = st == ""
match Lambda st = st == ""
match (Letter ch) [str] = str == ch
match (Letter ch) _ = False
match (Choice r1 r2) st = match r1 st || match r2 st
match (Concat r1 r2) st = or [match r1 s1 && match r2 s2 | (s1, s2)<- splitString st]
match (Star r) st = match Empty st || or [match r s1 && match (Star r) s2 | (s1, s2) <- frontSplit st]

splitString :: [a] -> [([a], [a])]
splitString str = [ splitAt n str | n <- [0 .. length str] ]

--frontSplit is the same as splitString, but it excludes the split ([], str)
frontSplit :: [a] -> [([a], [a])]
frontSplit str = [splitAt n str | n <- [1 .. length str]]

matchPrefix :: Regex -> [Char] -> [String]
matchPrefix Empty str = []
matchPrefix Lambda str = [str]
matchPrefix (Letter ch) [] = []
matchPrefix (Letter ch) (strChar:rst) = [rst | ch == strChar]
matchPrefix (Choice r1 r2) str = matchPrefix r1 str ++ matchPrefix r2 str
matchPrefix (Concat r1 r2) str = [ s3 | (s1, s2, s3) <- splitThree str, match r1 s1 && match r2 s2 ]
matchPrefix (Star r) str = str : [ s3 | (s1, s2, s3) <- splitThree str, match r s1 && match (Star r) s2]
--matchPrefix (Star r) str = matchPrefix (Choice Lambda (Concat r (Star r))) str
--helper functions for match prefix
--generate all the possible tuples for s1 ++ s2 ++ s3 = str.
--Repeated characters are treated as different. (Hello generates Hel twice since two ls.)
splitThree :: [a] -> [([a], [a], [a])]
splitThree str = [(xs, ys, zs) | (xs, rst) <- splitString str, (ys, zs) <-  splitString rst]

data Token = LetterOp Char | ConcatOp | ChoiceOp | StarOp | EmptyOp | LambdaOp
            | LPar | RPar  | REG Regex
            deriving Show

parser :: String -> Regex
parser s = case sr [] (lexer s) of
    [REG r] -> r
    e -> error ("Parse error: " ++ show e)

sr :: [Token] -> [Token] -> [Token]
--letter
sr (LetterOp x: s) ts = sr (REG(Letter x): s) ts
--concat
sr (REG r2 : ConcatOp : REG r1 : s) ts = sr (REG (Concat r1 r2) : s) ts
--choice
sr (REG r2 : ChoiceOp : REG r1 : s) ts = sr (REG (Choice r1 r2) : s) ts
--star
sr (StarOp : REG r : s) ts = sr (REG (Star r) : s) ts
--lambda
sr (LambdaOp : s) ts = sr (REG Lambda : s) ts
--empty
sr (EmptyOp : s) ts = sr (REG Empty : s) ts
--pars
sr (RPar : REG r : LPar : s) ts = sr (REG r : s) ts
--basecase
sr s (t:ts) = sr (t:s) ts
sr s [] = s


lexer :: String -> [Token]
--basecase
lexer "" = []
lexer (s : ss) | isSpace s = lexer ss
--concat
lexer (';':  ss) = ConcatOp : lexer ss
--choice
lexer ('|': ss)   = ChoiceOp : lexer ss
--star
lexer ('*': ss)  = StarOp : lexer ss
--lambda
lexer ('\\': ss) = LambdaOp : lexer ss
--punctuation
lexer ('(': ss) = LPar : lexer ss
lexer (')': ss) = RPar : lexer ss
--letter
lexer (s : ss) = LetterOp s : lexer ss