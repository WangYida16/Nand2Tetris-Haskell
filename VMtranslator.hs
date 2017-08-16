import System.IO
import Control.Exception
import Data.List
import Data.Char


{-注意
  测试文件中的SP值并不一致
  本脚本中还未加入修改SP值初始化的功能
-}

arithmetic = ["add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not"]

main0 = do
    print "Please input the file you want to translate:"
    fileName <- getLine
    source <- openFile fileName ReadMode
    outfile <- openFile "out.asm" AppendMode
    rawContent <- hGetContents source 
    let content0 = [s | s <- (map clear (lines rawContent)), length s /= 0]
        content1 = zip [0..] content0
        --得到有意义的行
        --TODO
        output0 = unlines $  map (\p -> deal p fileName) content1    --content1 待修改
        output  = unlines $ [s | s <- lines output0, length s /= 0]
    hPutStr outfile (unlines ["@256","D=A","@SP","M=D","@Sys.init","0;JMP"])
    hPutStr outfile output
    hClose source
    hClose outfile

main = do
    print "Please input the file you want to translate:"
    fileName <- getLine
    source <- openFile fileName ReadMode
    outfile <- openFile "out.asm" AppendMode
    rawContent <- hGetContents source 
    let content0 = [s | s <- (map clear (lines rawContent)), length s /= 0]
        content1 = zip [0..] content0
        --得到有意义的行
        --TODO
        output0 = unlines $  map (\p -> deal p fileName) content1    --content1 待修改
        output  = unlines $ [s | s <- lines output0, length s /= 0]
    --hPutStr outfile (unlines ["@256","D=A","@SP","M=D","@Sys.init","0;JMP"])
    hPutStr outfile output
    hClose source
    hClose outfile

clear :: String -> String
clear s 
    | length s == 0 = ""
    | s!!0 == '/' && s!!1 == '/' = ""
    | otherwise = [a | a <- ss]
        where ss = if "//" `isInfixOf` s then let str = (head (dropWhile (\xs->not("//"`isInfixOf`xs)) (inits s))) in take (length str - 2) str else s

deal :: (Integer, String) -> String -> String
deal s fileName
    | ((words (snd s))!!0)`elem`arithmetic = dealAri s fileName
    | (words (snd s))!!0 == "push" = dealPush s fileName
    | (words (snd s))!!0 == "pop" = dealPop s fileName
    | (words (snd s))!!0 == "label" = dealLabel s fileName
    | (words (snd s))!!0 == "goto" = dealGoTo s fileName
    | (words (snd s))!!0 == "if-goto" = dealIfGo s fileName
    | (words (snd s))!!0 == "function" = dealFunction s fileName
    | (words (snd s))!!0 == "call" = dealCall s fileName
    | (words (snd s))!!0 == "return" = dealReturn s fileName
    | otherwise = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

dealAri :: (Integer, String) -> String -> String
dealAri s fileName
    | command == "add" = unlines ["@SP","M=M-1","@SP","A=M","D=M","@SP","M=M-1","@SP","A=M","A=M","D=D+A","@SP","A=M","M=D","@SP","M=M+1"]
    | command == "sub" = unlines ["@SP","M=M-1","@SP","A=M","D=M","@SP","M=M-1","@SP","A=M","A=M","D=D-A","D=-D","@SP","A=M","M=D","@SP","M=M+1"]
    | command == "neg" = unlines ["@SP","M=M-1","@SP","A=M","M=-M","@SP","M=M+1"]
    | command == "and" = unlines ["@SP","M=M-1","@SP","A=M","D=M","@SP","M=M-1","@SP","A=M","A=M","D=D&A","@SP","A=M","M=D","@SP","M=M+1"]
    | command == "or"  = unlines ["@SP","M=M-1","@SP","A=M","D=M","@SP","M=M-1","@SP","A=M","A=M","D=D|A","@SP","A=M","M=D","@SP","M=M+1"]
    | command == "not" = unlines ["@SP","M=M-1","@SP","A=M","M=!M","@SP","M=M+1"]
    | command == "eq"  = unlines ["@SP","M=M-1","@SP","A=M","D=M","@SP","M=M-1","@SP","A=M","A=M","D=D-A","@NormalSymbol-"++fileName++"-"++(show num)++"-1","D;JEQ","@SP","A=M","M=0","@SP","M=M+1","@NormalSymbol-"++fileName++"-"++(show num)++"-2","0;JMP","(NormalSymbol-"++fileName++"-"++(show num)++"-1)","@SP","A=M","M=-1","@SP","M=M+1","(NormalSymbol-"++fileName++"-"++(show num)++"-2)"]
    | command == "gt"  = unlines ["@SP","M=M-1","@SP","A=M","D=M","@SP","M=M-1","@SP","A=M","A=M","D=D-A","D=-D","@NormalSymbol-"++fileName++"-"++(show num)++"-1","D;JGT","@SP","A=M","M=0","@SP","M=M+1","@NormalSymbol-"++fileName++"-"++(show num)++"-2","0;JMP","(NormalSymbol-"++fileName++"-"++(show num)++"-1)","@SP","A=M","M=-1","@SP","M=M+1","(NormalSymbol-"++fileName++"-"++(show num)++"-2)"]
    | command == "lt"  = unlines ["@SP","M=M-1","@SP","A=M","D=M","@SP","M=M-1","@SP","A=M","A=M","D=D-A","D=-D","@NormalSymbol-"++fileName++"-"++(show num)++"-1","D;JLT","@SP","A=M","M=0","@SP","M=M+1","@NormalSymbol-"++fileName++"-"++(show num)++"-2","0;JMP","(NormalSymbol-"++fileName++"-"++(show num)++"-1)","@SP","A=M","M=-1","@SP","M=M+1","(NormalSymbol-"++fileName++"-"++(show num)++"-2)"]
        where command = (words (snd s))!!0
              num = fst s

dealPush :: (Integer, String) -> String -> String
dealPush p fileName
    | second == "constant" = unlines ["@"++third,"D=A","@SP","A=M","M=D","@SP","M=M+1"]
    | second == "argument" = unlines ["@"++third,"D=A","@ARG","A=M","A=D+A","D=M","@SP","A=M","M=D","@SP","M=M+1"]
    | second == "this"     = unlines ["@"++third,"D=A","@THIS","A=M","A=D+A","D=M","@SP","A=M","M=D","@SP","M=M+1"]
    | second == "that"     = unlines ["@"++third,"D=A","@THAT","A=M","A=D+A","D=M","@SP","A=M","M=D","@SP","M=M+1"]
    | second == "local"    = unlines ["@"++third,"D=A","@LCL","A=M","A=D+A","D=M","@SP","A=M","M=D","@SP","M=M+1"]
    | second == "temp"     = unlines ["@"++third,"D=A","@5","A=D+A","D=M","@SP","A=M","M=D","@SP","M=M+1"]
    | second == "static"   = unlines ["@"++fileName++"."++third,"D=M","@SP","A=M","M=D","@SP","M=M+1"]
    | second == "pointer"  = unlines ["@"++third,"D=A","@3","D=D+A","A=D","D=M","@SP","A=M","M=D","@SP","M=M+1"]
        where first  = (words (snd p))!!0
              second = (words (snd p))!!1
              third  = (words (snd p))!!2
              num = fst p

dealPop :: (Integer, String) -> String -> String
dealPop p fileName
    | second == "argument" = unlines ["@"++third,"D=A","@ARG","A=M","D=D+A","@R14","M=D","@SP","M=M-1","A=M","D=M","@R13","M=D","@R14","A=M","M=D"]
    | second == "this"     = unlines ["@"++third,"D=A","@THIS","A=M","D=D+A","@R14","M=D","@SP","M=M-1","A=M","D=M","@R13","M=D","@R14","A=M","M=D"]
    | second == "that"     = unlines ["@"++third,"D=A","@THAT","A=M","D=D+A","@R14","M=D","@SP","M=M-1","A=M","D=M","@R13","M=D","@R14","A=M","M=D"]
    | second == "local"    = unlines ["@"++third,"D=A","@LCL","A=M","D=D+A","@R14","M=D","@SP","M=M-1","A=M","D=M","@R13","M=D","@R14","A=M","M=D"]
    | second == "temp"     = unlines ["@"++third,"D=A","@5","D=D+A","@R13","M=D","@SP","M=M-1","A=M","D=M","@R13","A=M","M=D"]
    | second == "static"   = unlines ["@"++fileName++"."++third,"D=A","@R13","M=D","@SP","M=M-1","A=M","D=M","@R13","A=M","M=D"]
    | second == "pointer"  = unlines ["@"++third,"D=A","@3","D=D+A","@R13","M=D","@SP","M=M-1","A=M","D=M","@R13","A=M","M=D"]
        where first  = (words (snd p))!!0
              second = (words (snd p))!!1
              third  = (words (snd p))!!2
              num = fst p

dealLabel :: (Integer, String) -> String -> String
dealLabel p fileName = "(VMSymbol-"++fileName++"-"++((words (snd p))!!1)++")"

dealGoTo :: (Integer, String) -> String -> String
dealGoTo p fileName = unlines ["@VMSymbol-"++fileName++"-"++((words (snd p))!!1), "0;JMP"]

dealIfGo :: (Integer, String) -> String -> String
dealIfGo p fileName = unlines ["@SP","M=M-1","A=M","D=M","@VMSymbol-"++fileName++"-"++((words (snd p))!!1),"D;JNE"]

dealFunction :: (Integer, String) -> String -> String
dealFunction p fileName = unlines (["("++fName++")"] ++ (repeatArgs n) )
        where fName = (words (snd p))!!1
              n = read((words (snd p))!!2) :: Integer

repeatArgs :: Integer -> [String]
repeatArgs n
    | n == 0 = []
    | n == 1 = ["@0","D=A","@SP","A=M","M=D","@SP","M=M+1"]
    | otherwise = ["@0","D=A","@SP","A=M","M=D","@SP","M=M+1"] ++ repeatArgs (n-1)

dealCall :: (Integer, String) -> String -> String
dealCall p fileName = unlines ["@return."++fileName++"-"++num,"D=A","@SP","A=M","M=D","@SP","M=M+1", "@LCL","D=M","@SP","A=M","M=D","@SP","M=M+1",
                       "@ARG","D=M","@SP","A=M","M=D","@SP","M=M+1", "@THIS","D=M","@SP","A=M","M=D","@SP","M=M+1",
                       "@THAT","D=M","@SP","A=M","M=D","@SP","M=M+1", "@SP","D=M","@5","D=D-A","@"++n,"D=D-A","@ARG","M=D",
                       "@SP","D=M","@LCL","M=D", "@"++f,"0;JMP", "("++"return."++fileName++"-"++num++")"]
        where f = (words (snd p))!!1
              n = (words (snd p))!!2
              num = show(fst p)

dealReturn :: (Integer, String) -> String -> String
dealReturn p fileName = unlines ["@LCL","D=M","@FRAME","M=D",  "@5","A=D-A","D=M","@RET","M=D",
                         "@SP","M=M-1","A=M","D=M","@ARG","A=M","M=D",  "@ARG","D=M","@SP","M=D+1",
                         "@FRAME","D=M","D=D-1","@FRAME","M=D","A=D","D=M","@THAT","M=D",
                         "@FRAME","D=M","D=D-1","@FRAME","M=D","A=D","D=M","@THIS","M=D",
                         "@FRAME","D=M","D=D-1","@FRAME","M=D","A=D","D=M","@ARG","M=D",
                         "@FRAME","D=M","D=D-1","@FRAME","M=D","A=D","D=M","@LCL","M=D",
                         "@RET","A=M","0;JMP"]





