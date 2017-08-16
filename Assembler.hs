import System.IO
import Control.Exception
import Data.List
import Data.Char

--Ԥ�������
preSyb = [("SP","000000000000000"),("LCL","000000000000001"),("ARG","000000000000010"),("THIS","000000000000011"),("THAT","000000000000100"),
          ("SCREEN","100000000000000"),("KBD","110000000000000"),("R0","000000000000000"),("R1","000000000000001"),("R2","000000000000010"),
          ("R3","000000000000011"),("R4","000000000000100"),("R5","000000000000101"),("R6","000000000000110"),("R7","000000000000111"),
          ("R8","000000000001000"),("R9","000000000001001"),("R10","000000000001010"),("R11","000000000001011"),("R12","000000000001100"),
          ("R13","000000000001101"),("R14","000000000001110"),("R15","000000000001111")]

jumpSyb = [("","000"),("JGT","001"),("JEQ","010"),("JGE","011"),("JLT","100"),("JNE","101"),("JLE","110"),("JMP","111")]

destSyb = [("M","001"),("D","010"),("MD","011"),("A","100"),("AM","101"),("AD","110"),("AMD","111"),("","000")]

compSyb = [("0","0101010"),("1","0111111"),("-1","0111010"),("D","0001100"),("A","0110000"),("!D","0001101"),("!A","0110001"),("-D","0001111"),
           ("-A","0110011"),("D+1","0011111"),("A+1","0110111"),("D-1","0001110"),("A-1","0110010"),("D+A","0000010"),("D-A","0010011"),
           ("A-D","0000111"),("D&A","0000000"),("D|A","0010101"),("M","1110000"),("!M","1110001"),("-M","1110011"),("M+1","1110111"),
           ("M-1","1110010"),("D+M","1000010"),("D-M","1010011"),("M-D","1000111"),("D&M","1000000"),("D|M","1010101")]

--IO�����Լ����ű��滻
main = do
    print "Please input the file you want to assemble:"
    fileName <- getLine
    source <- openFile fileName ReadMode
    outfile <- openFile "out.hack" WriteMode
    rawContent <- hGetContents source 
    let content0 = [s | s <- (map clear (lines rawContent)), length s /= 0]
        --content0: ȥ��ע��\����\�ո�, �õ�"ȫ��"
        content1 = [s | s <- content0, s!!0 /= '(']
        --content1: ȥ����ǩ����, �õ�"����"
        combine1 = zip [0..] [s | s <- content0]
        --combine1: ��"ȫ��"���
        combine2 = zipWith (\n2 (n1,s) -> (n2, n1, s)) [0..] [s | s <- combine1, (snd s)!!0 /= '(' ] 
        --combine2: ���ѱ��(1)��"ȫ��"ȥ����ǩ����, ���ٴα��(2), ����ÿһ��"����"���������, �ڶ�����"ȫ��"���, ��һ����"����"���
        symbols = [init (tail s) | s <- content0, s!!0 == '(']
        --symbols: ��ǩ�����б�
        symTab = [((findKey'' (findNext ("("++sym++")") combine1) combine2), sym) | sym <- symbols ]
        --symTab: ��ǩ���� �Լ� ���Ƕ�Ӧ�ĵ�ַ���
        variables = nub [tail s | s <- content0, s!!0 == '@', not((tail s)`elem`symbols), not((tail s)`elem`(map fst preSyb)), isAlpha(s!!1)]
        --variables: �Է��ŵ���ʽ�������ʱ����
        varTab = zipWith (\n s -> (show n, s)) [16..] variables
        --varTab: ���� �Լ� ���Ƕ�Ӧ���ڴ���
        content2 = map (\x -> if (x !! 0 == '@' && (tail x)`elem`symbols) then "@"++(findKey (tail x) symTab) else x) content1
        --�滻 ��ǩ���� Ϊ ��Ӧ�ĵ�ַ
        content3 = map (\x -> if (x!!0) == '@' && (tail x)`elem`variables then "@"++(findKey (tail x) varTab) else x) content2
        --�滻 �������� Ϊ ������ڴ�
        output = unlines (map deal content3)
    hPutStr outfile output
    hClose source
    hClose outfile

--�����������,���ಢ����
deal :: String -> String
deal ss
    | ss!!0 == '@' = "0" ++ enlarge (dealA (tail ss))
    | '='`elem`ss || ';'`elem`ss  = dealC (ss)
    | otherwise  = "ErrorInThisLine"  

--ȥ���ո��ע��
clear :: String -> String
clear s 
    | length s == 0 = ""
    | s!!0 == '/' && s!!1 == '/' = ""
    | otherwise = [a | a <- ss, a /= ' ']
        where ss = if "//" `isInfixOf` s then let str = (head (dropWhile (\xs->not("//"`isInfixOf`xs)) (inits s))) in take (length str - 2) str else s

--���ദ��Aָ��
dealA :: String -> String
dealA ss
    | isAlpha (ss!!0) && ss`elem`(map fst preSyb) = findKey' ss preSyb 
    | isDigit (ss!!0) = secondO ss
    | otherwise = "!!!"++ss

--������ת��--�ⲿ--�������ַ���ת��
secondO :: String -> String
secondO ss = reverse (secondI ((read ss :: Int) ))

--������ת��--�ڲ�--��������
secondI :: Int -> String
secondI n
    | n `div` 2 == 0 = [intToDigit n]
    | otherwise = (intToDigit(n `mod` 2) ) : (secondI (n `div` 2))

{-
  �������������е����
-}
--���Ҽ�ֵ
findKey :: String -> [(String, String)] ->  String
findKey key = foldr (\(k,v) acc -> if v == key then k else acc) ""

--���Ҽ�ֵ2
findKey' :: String -> [(String,String)] -> String
findKey' key = foldr (\(k,v) acc -> if k == key then v else acc) ""

--���Ҽ�ֵ3
findKey'' :: Integer -> [(Integer, Integer, String)] -> String
findKey'' key = foldr (\(n2, n1, s) acc -> if n1 == key then show(n2) else acc ) ""

--����Cָ��
dealC :: String -> String
dealC ss = "111" ++ comp co ++ dest de ++ jump ju
        where co = if '='`elem`ss then tail $ snd (break (=='=') ss) else fst (break (==';') ss)
              de = if '='`elem`ss then fst (break (=='=') ss) else ""
              ju = if '='`elem`ss then "" else tail $ snd (break (==';') ss)

dest :: String -> String
dest ss = findKey' ss destSyb

comp :: String -> String
comp ss = findKey' ss compSyb

jump :: String -> String
jump ss = findKey' ss jumpSyb

--����λ��
enlarge :: String -> String
enlarge ss
    | length ss == 15 = ss
    | otherwise = replicate (15 - length ss) '0' ++ ss

--�õ���ǩ���ŵ���һ�з���ת�еı��
findNext :: String -> [(Integer, String)] -> Integer
findNext s0 all = if (snd s1)!!0 == '(' then findNext (snd s1) all else (fst s1)
                    where s1 = all!!fromIntegral((getIndex all s0) + 1)

--�õ�����
getIndex :: [(Integer, String)] -> String -> Integer
getIndex all s = foldr (\(k,v) acc -> if v == s then k else acc) 0 all
















