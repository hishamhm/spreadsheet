\documentclass[a4paper]{article}
\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%include Xl_format.lhs

\begin{document}

\title{Demonstration of the spreadsheet interpreter}
\author{Hisham Muhammad}

\maketitle{}

\section{Introduction}

We present here a demonstration of the spreadsheet interpreter in use. This
appendix is a Literate Haskell program including the complete source code of
the demonstration.

This demonstration presents a series of tests, many of which correspond to
sections of the OpenDocument specification for the @.ods@ format
[***reference***] and the ISO Open Office XML specification for the @.xlsx@
format [***reference***], as well as our own additional tests that cover some
unspecified behavior.

Finally, we run the tests. As an illustration of the execution, the first
example below produces the following output:

\begin{footnotesize}\begin{verbatim}
Values:
  |A        |B        |C        |D        |E        |F        |G        |H        |I        |J        
 1|       15|       15|"B"      |       75|       30|      105|     1015|       10|      -20|       30
 2|       30|       15|        1|         |         |         |         |         |       20|         
 3|         |         |         |         |         |         |         |         |         |         
 4|         |         |         |         |         |         |         |         |         |         
 5|         |         |"#VALUE!"|      115|         |       15|         |         |         |         
 6|         |         |         |      130|         |       16|         |         |         |         
 7|         |         |         |         |         |         |         |         |         |         
 8|         |         |         |         |         |         |"#VALUE!"|         |         |         
 9|         |         |         |         |         |         |         |         |         |         
10|       10|         |         |         |         |         |         |         |         |         
11|"10"     |         |         |         |         |         |         |         |         |         
12|False    |         |         |         |         |         |         |         |         |         
13|True     |         |         |         |         |         |         |         |         |         
14|True     |         |         |         |         |         |         |         |         |         
15|"#DIV/0!"|         |         |         |         |         |         |         |         |         
16|"#VALUE!"|         |         |         |         |         |         |         |         |         
17|"#DIV/0!"|         |         |         |         |         |         |         |         |         
\end{verbatim}\end{footnotesize}

This program imports the interpreter defined in Chapter [*** number ***] as a
module, as well as some standard modules from the Haskell Platform. We also
use one additional module for tabular pretty-printing of the output:
|Text.PrettyPrint.Boxes|, available from Hackage, the Haskell community's
package repository [*** reference/link ***].

\begin{code}

import XlInterpreter
import Data.Char (chr, ord)
import Data.Map.Strict as Map (foldlWithKey, empty, lookup, toList, (!))
import Text.PrettyPrint.Boxes as Box (render, hcat, vcat, text)
import Text.PrettyPrint.Boxes as Alignment (left, right)

\end{code}

\section{Formatting}

In order to produce a more readable output, we define the instance |Show| for
our |XlState| type, using the |Text.PrettyPrint| package to produce tabular
outputs.

\begin{code}

instance Show XlState where
   show (XlState cells values) = 
      "\nCells:\n" ++ listCells ++ "\nValues:\n" ++ tableValues ++ "\n" ++ show values ++ "\n"
         where
            maxRow       =  Map.foldlWithKey (\mx (XlRC (XlAbs r) _) _ -> max r mx) 0 values
            maxCol       =  Map.foldlWithKey (\mx (XlRC _ (XlAbs c)) _ -> max c mx) 0 values
            listCells    =  Box.render
                            $ Box.vcat Alignment.left 
                            $ map Box.text 
                            $ map show (Map.toList cells)
            tableValues  =  Box.render
                            $ Box.hcat Alignment.left 
                            $ numsCol : map doCol [0..maxCol]
            numsCol      =  Box.vcat Alignment.right
                            $ map Box.text
                            $ " " : map show [1..(maxRow + 1)]
            doCol c      =  Box.vcat Alignment.left
                            $ Box.text ['|', chr (c + 65)] : 
                              map (\s -> Box.text ('|' : doRow s c)) [0..maxRow]
            lpad m xs    =  reverse $ take m $ reverse $ (take m $ repeat ' ') ++ (take m xs)
            doRow r c    =  case Map.lookup ((XlRC (XlAbs r) (XlAbs c))) values of
                            Just (XlNumber n)  -> lpad 9 (num2str n)
                            Just v             -> show v
                            Nothing            -> ""

\end{code}

\section{A test driver}

We construct below a test driver function that runs test cases and compares
their results to expected values.

\begin{code}
runTest :: String -> [(XlEvent, XlValue)] -> IO ()
runTest name operations =
   let
      env@(XlState cells values) = runEvents (XlWorksheet Map.empty) (map fst operations)
      value :: String -> XlValue
      value a1 = values ! (toRC a1)
   
      failures = filter (\v -> v /= Nothing) $ map doCheck operations
         where
            doCheck (op, value) =
               case op of
                  XlSetFormula rc fml ->
                     if values ! rc == value
                     then Nothing
                     else Just (rc, value)
                  XlSetArrayFormula rcFrom rcTo fml ->
                     if values ! rcFrom == value
                     then Nothing else
                     Just (rcFrom, value)
   in do
      putStrLn ""
      print name
      print env
      if null failures
      then putStrLn "OK! :-D"
      else
         do
            putStrLn "Failed: "
            print failures
\end{code}

We employ a few shortcuts to write down formulas more tersely on paper:

\begin{code}
str s = XlString s
num n = XlNumber n
err e = XlError e
boo b = XlBool b
lnum n = XlLit (XlNumber n)
lstr s = XlLit (XlString s)
lmtx mx = XlLit (XlMatrix (map (map XlNumber) mx))
lmtxs mx = XlLit (XlMatrix (map (map XlString) mx))
fun f args = XlFun f args
ref a1 = XlRef (toRC a1)
range a1 b2 = XlRng (toRC a1) (toRC b2)
toRC (l:num) = XlRC (XlAbs ((read num) - 1)) (XlAbs ((ord l) - 65))
addF rc f v = (XlSetFormula (toRC rc) f, v)
addAF rcFrom rcTo f v = (XlSetArrayFormula (toRC rcFrom) (toRC rcTo) f, v)
sumSqrt l = num $ foldr (+) 0 (map sqrt l)
\end{code}

Each invocation of |runTest| below is a new spreadsheet, where |addF| and
|addAF| are events adding formulas and array formulas to cells. The last
argument is the expected value. All tests here produce the indicated values.

This batch of tests aims to document the specific behavior of the interpreter
written in Chapter [*** number ***] and also serves as a list of corner cases
which expose incompatibilities between real-world spreadsheet applications.

\begin{code}

main :: IO ()
main = 
   do
   runTest "Example" [
      addF "A1" (lnum 15) (num 15),
      addF "B1" (lnum 0) (num 15),
      addF "A2" (fun "+" [ref "A1", ref "B1"]) (num 30),
      addF "B1" (ref "A1") (num 15),
      addF "C1" (lstr "B") (str "B"),
      addF "C2" (lnum 1) (num 1),
      addF "B2" (fun "INDIRECT" [fun "&" [ref "C1", ref "C2"]]) (num 15),
      addF "D1" (fun "SUM" [range "A1" "B2"]) (num 75),
      addF "E1" (fun "SUM" [range "B1" "B2"]) (num 30),
      addF "F1" (fun "SUM" [range "D1" "E1"]) (num 105),
      
      addF "H1" (lnum 10) (num 10),
      addF "I1" (lnum (-20)) (num (-20)),
      addF "J1" (lnum 30) (num 30),
      addF "I2" (fun "ABS" [range "H1" "J1"]) (num 20),
      addF "G8" (fun "ABS" [range "H1" "J1"]) (err "#VALUE!"),
      
      addF "A10" (lnum 10) (num 10),
      addF "A11" (lstr "10") (str "10"),
      addF "A12" (fun "=" [ref "A10", ref "A11"]) (boo False),
      addF "A13" (fun "=" [ref "A10", lnum 10]) (boo True),
      addF "A14" (fun "=" [ref "A13", lnum 1]) (boo True),
      
      addF "A15" (fun "/" [lnum 1, lnum 0]) (err "#DIV/0!"),
      addF "A16" (fun "=" [ref "G8", ref "A15"]) (err "#VALUE!"),
      addF "A17" (fun "=" [ref "A15", ref "G8"]) (err "#DIV/0!"),
      
      addF "G1" (fun "+" [lnum 1000, range "A1" "A2"]) (num 1015),
      
      addF "C5" (range "A1" "A2") (err "#VALUE!"),
      addAF "F5" "F6" (lmtx [[15], [16]]) (num 15),
      addAF "D5" "D6" (fun "+" [range "A1" "A2", lnum 100]) (num 115)]

   runTest "OASIS 3.3 1.1) Note 1" [
      addF "A1" (fun "ABS" [lmtx [[-3, -4]]]) (num 3),
      addF "A2" (fun "ABS" [lmtx [[-3], [-4]]]) (num 3),
      addF "A3" (fun "ABS" [lmtx [[-3, -4], [-6, -8]]]) (num 3),
      addF "A4" (lmtx [[1, 2, 3], [4, 5, 6]]) (num 1)]

   runTest "OASIS 3.3 1.2.1) Notes 2 and 3" [
      addF "A1" (lnum (-10)) (num (-10)),
      addF "B1" (lnum (-20)) (num (-20)),
      addF "C1" (lnum (-30)) (num (-30)),
      addF "B2" (fun "ABS" [range "A1" "C1"]) (num 20),
      addF "D4" (fun "ABS" [range "A1" "C1"]) (err "#VALUE!")]
   
   runTest "OASIS 3.3 2.1.4) Note 5.1" [
      addAF "A1" "B3" (lmtx [[1,2],[3,4],[5,6]]) (num 1),
      addF "C3" (ref "B2") (num 4)]

   runTest "OASIS 3.3 2.1.4) Note 5.2" [
      addAF "A1" "B3" (lmtx [[1],[3],[5]]) (num 1),
      addF "C3" (ref "B2") (num 3)]

   runTest "OASIS 3.3 2.1.4) Note 5.3" [
      addAF "A1" "B3" (lmtx [[2,4]]) (num 2),
      addF "C3" (ref "B2") (num 4)]

   runTest "OASIS 3.3 2.1.4) Note 5.3" [
      addAF "A1" "C4" (lmtx [[1,2],[3,4],[5,6]]) (num 1),
      addF "D1" (ref "C1") (err "#N/A"),
      addF "D2" (ref "A4") (err "#N/A")]

   runTest "OASIS 3.3 2.1.4) Note 6" [
      addAF "A1" "B2" (lmtx [[1,2],[3,4],[5,6]]) (num 1),
      addF "D1" (ref "B3") (XlEmpty)]

   runTest "OASIS 3.3 2.2.1) Note 7" [
      addF "A1" (lnum 10) (num 10),
      addF "A2" (lnum 20) (num 20),
      addF "B3" (fun "INDIRECT" [lmtxs [["A1", "A2"]]]) (num 10),
      addAF "B4" "C4" (fun "INDIRECT" [lmtxs  [["A1", "A2"]]]) (num 10),
      addF "B5" (fun "SUM" [fun "INDIRECT" [lmtxs [["A1", "A2"]]]]) (num 10),
      addAF "B6" "B6" (fun "SUM" [fun "INDIRECT" [lmtxs [["A1", "A2"]]]]) (num 30),
      addF "B7" (fun "SUM" [fun "SQRT" [range "A1" "A2"]]) (err "#VALUE!"),
      addAF "B8" "B8" (fun "SUM" [fun "SQRT" [range "A1" "A2"]]) (sumSqrt [10,20]),
      addF "C7" (fun "SQRT" [range "A1" "A2"]) (err "#VALUE!"),
      addAF "C8" "C9" (fun "SQRT" [range "A1" "A2"]) (num (sqrt 10)),
      addF "D3" (lmtx [[10, 20]]) (num 10),
      addAF "D4" "E4" (lmtx [[10, 20]]) (num 10),
      addF "D5" (fun "SUM" [lmtx [[10,20]]]) (num 30),
      addAF "D6" "D6" (fun "SUM" [lmtx [[10,20]]]) (num 30),
      addF "D7" (fun "SUM" [fun "SQRT" [lmtx [[10,20]]]]) (num (sqrt 10)),
      addAF "D8" "D8" (fun "SUM" [fun "SQRT" [lmtx [[10,20]]]]) (sumSqrt [10,20]),
      addF "E7" (fun "SQRT" [lmtx [[10,20]]]) (num (sqrt 10)),
      addAF "E8" "E9" (fun "SQRT" [lmtx [[10],[20]]]) (num (sqrt 10))]

   runTest "OASIS 3.3 2.2.1) Note 8.1" [
      addAF "A1" "C1" (fun "+" [lmtx [[1,2]], lmtx [[3,4,5]]]) (num 4),
      addF "A2" (ref "A1") (num 4),
      addF "B2" (ref "B1") (num 6),
      addF "C2" (ref "C1") (err "#N/A")]

   runTest "OASIS 3.3 2.2.1) Note 8.2" [
      addAF "A1" "B1" (fun "+" [lmtx [[1]], lmtx [[1,2]]]) (num 2),
      addF "A2" (ref "A1") (num 2),
      addF "B2" (ref "B1") (num 3)]
   
   runTest "OASIS 3.3 2.2.3.1) Note 9.1" [
      addAF "A1" "C2" (fun "+" [lnum 1, lmtx [[1,2,3],[4,5,6]]]) (num 2),
      addF "D1" (ref "A1") (num 2),
      addF "E1" (ref "B1") (num 3),
      addF "F1" (ref "C1") (num 4),
      addF "D2" (ref "A2") (num 5),
      addF "E2" (ref "B2") (num 6),
      addF "F2" (ref "C2") (num 7)]
   
   runTest "OASIS 3.3 2.2.3.1) Note 9.2" [
      addAF "A1" "C2" (fun "+" [lmtx [[1]], lmtx [[1,2,3],[4,5,6]]]) (num 2),
      addF "D1" (ref "A1") (num 2),
      addF "E1" (ref "B1") (num 3),
      addF "F1" (ref "C1") (num 4),
      addF "D2" (ref "A2") (num 5),
      addF "E2" (ref "B2") (num 6),
      addF "F2" (ref "C2") (num 7)]

   runTest "OASIS 3.3 2.2.3.1) Note 10" [
      addAF "A1" "B2" (fun "+" [lmtx [[1],[2]], lmtx [[10,20],[30,40]]]) (num 11),
      addF "D1" (ref "A1") (num 11),
      addF "E1" (ref "B1") (num 21),
      addF "D2" (ref "A2") (num 32),
      addF "E2" (ref "B2") (num 42)]

   runTest "OASIS 3.3 2.2.3.1) Note 11" [
      addAF "A1" "B2" (fun "+" [lmtx [[1,2]], lmtx [[10,20],[30,40]]]) (num 11),
      addF "D1" (ref "A1") (num 11),
      addF "E1" (ref "B1") (num 22),
      addF "D2" (ref "A2") (num 31),
      addF "E2" (ref "B2") (num 42)]
   
   runTest "OASIS 3.3 2.2.3.1) Note 12" [
      addAF "A1" "B2" (fun "+" [lmtx [[1,2]], lmtx [[10],[20]]]) (num 11),
      addF "D1" (ref "A1") (num 11),
      addF "E1" (ref "B1") (num 12),
      addF "D2" (ref "A2") (num 21),
      addF "E2" (ref "B2") (num 22)]

   runTest "OASIS 3.3 2.2.3.1) Note 13" [
      addAF "A1" "C1" (fun "MID" [lstr "abcd", lmtx [[1,2]], lmtx [[1,2,3]]]) (str "a"),
      addF "A2" (ref "A1") (str "a"),
      addF "B2" (ref "B1") (str "bc"),
      addF "C2" (ref "C1") (err "#VALUE!")]
   
   runTest "ISO/IEC 29500:1 2012 page 2040 ex.1" [
      addF "B2" (lnum 1) (num 1),
      addF "B3" (lnum 2) (num 2),
      addF "B4" (lnum 3) (num 3),
      addF "C2" (lnum 4) (num 4),
      addF "C3" (lnum 5) (num 5),
      addF "C4" (lnum 6) (num 6),
      addAF "D2" "D4" (fun "+" [fun "*" [range "B2" "B4", range "C2" "C4"], lnum 10.5]) (num 14.5),
      addF "E2" (ref "D2") (num 14.5),
      addF "E3" (ref "D3") (num 20.5),
      addF "E4" (ref "D4") (num 28.5)]
   
   runTest "ISO/IEC 29500:1 2012 page 2040 exs.2 and 3" [
      addF "A1" (fun "SQRT" [lmtx [[1,2,3,4]]]) (num 1),
      addAF "B1" "B1" (fun "SQRT" [lmtx [[1,2,3,4]]]) (num 1),
      addAF "C1" "G1" (fun "SQRT" [lmtx [[1,2,3,4]]]) (num 1),
      addF "C2" (ref "C1") (num (sqrt 1)),
      addF "D2" (ref "D1") (num (sqrt 2)),
      addF "E2" (ref "E1") (num (sqrt 3)),
      addF "F2" (ref "F1") (num (sqrt 4)),
      addF "G2" (ref "G1") (err "#N/A")]
   
   -- These results are consistent with LibreOffice, but not with the ISO document:
   runTest "ISO/IEC 29500:1 2012 page 2040 ex.4" [
      addF "A1" (fun "SUM" [fun "SQRT" [lmtx [[1,2,3,4]]]]) (num 1),
      addAF "A2" "A2" (fun "SUM" [fun "SQRT" [lmtx [[1,2,3,4]]]]) (sumSqrt [1,2,3,4])]
   
   runTest "more tests with array formulas" [
      addF "A1" (fun "SUM" [fun "SQRT" [lmtx [[1,2,3,4]]]]) (num 1),
      addAF "A2" "A2" (fun "SUM" [fun "SQRT" [lmtx [[1,2,3,4]]]]) (sumSqrt [1,2,3,4]),
      addAF "B1" "E1" (fun "SUM" [fun "SQRT" [lmtx [[1,2,3,4]]]]) (sumSqrt [1,2,3,4]),
      addAF "A3" "D3" (fun "SUM" [fun "+" [lmtx [[1,2,3,4]], lnum 100]]) (num 410)]
   
   runTest "A test with an array formula and a range" [
      addF "A1" (lnum 1) (num 1),
      addF "A2" (lnum 2) (num 2),
      addF "A3" (lnum 3) (num 3),
      addF "A4" (lnum 4) (num 4),
      addF "B5" (fun "SUM" [fun "+" [range "A1" "A4", lnum 100]]) (err "#VALUE!"),
      addAF "B6" "B6" (fun "SUM" [fun "+" [range "A1" "A4", lnum 100]]) (num 410)]
   
   -- These are not fully compatible with LibreOffice:
   runTest "Array formulas and nested functions" [
      addF "A3" (lstr "F") (str "F"),
      addF "B3" (fun "SUM" [fun "+" [lmtx [[1,2,3,4]], lnum 100]]) (num 101),
      addF "A4" (lstr "AF") (str "AF"),
      addAF "B4" "B4" (fun "SUM" [fun "+" [lmtx [[1,2,3,4]], lnum 100]]) (num 410),
      addF "A5" (lstr "F") (str "F"),
      addF "B5" (fun "SUM" [fun "+" [fun "ABS" [lmtx [[1,2,3,4]] ], lnum 100]]) (num 101),
      addF "A6" (lstr "AF") (str "AF"),
      addAF "B6" "B6" (fun "SUM" [fun "+" [fun "ABS" [lmtx [[1,2,3,4]] ], lnum 100]]) (num 101)]
   
   -- In the following test, all match LibreOffice except @D8@:
   runTest "Formula evaluation incompatibilities" [
      addF "A1" (lnum 10) (num 10),
      addF "A2" (lnum 20) (num 20),
      addF "B1" (lnum 20) (num 20),
      
      addF "D3" (fun "SUM" [fun "SQRT" [lmtx [[10,20]]]]) (num (sqrt 10)),
      addAF "D4" "D4" (fun "SUM" [fun "SQRT" [lmtx [[10,20]]]]) (sumSqrt [10,20]),
      addF "D5" (fun "SUM" [fun "SQRT" [range "A1" "A2"]]) (err "#VALUE!"),
      addAF "D6" "D6" (fun "SUM" [fun "SQRT" [range "A1" "A2"]]) (sumSqrt [10,20]),
      addF "D7" (fun "SUM" [fun "SQRT" [fun "INDIRECT" [lmtxs [["A1", "A2"]]]]]) (num (sqrt 10)),
      addAF "D8" "D8" (fun "SUM" [fun "SQRT" [fun "INDIRECT" [lmtxs [["A1", "A2"]]]]]) (num (sqrt 10)),
      addF "D9" (fun "SUM" [fun "INDIRECT" [lmtxs [["A1", "A2"]]]]) (num 10),
      addAF "D10" "D10" (fun "SUM" [fun "INDIRECT" [lmtxs [["A1", "A2"]]]]) (num 30),
      
      addF "D11" (fun "SQRT" [lmtx [[10,20]]]) (num (sqrt 10)),
      addAF "D12" "E12" (fun "SQRT" [lmtx [[10,20]]]) (num (sqrt 10)),
      addF "D13" (fun "SQRT" [range "A1" "B1"]) (err "#VALUE!"),
      addAF "D14" "E14" (fun "SQRT" [range "A1" "B1"]) (num (sqrt 10)),
      addF "D15" (fun "SQRT" [fun "INDIRECT" [lmtxs [["A1", "B1"]]]]) (num (sqrt 10)),
      addAF "D16" "E16" (fun "SQRT" [fun "INDIRECT" [lmtxs [["A1", "B1"]]]]) (num (sqrt 10)),
      addF "D17" (fun "INDIRECT" [lmtxs [["A1", "B1"]]]) (num 10),
      addAF "D18" "E18" (fun "INDIRECT" [lmtxs [["A1", "B1"]]]) (num 10)]
   
   runTest "Circular references" [
      addF "B1" (lnum 100) (err "#LOOP!"),
      addF "A1" (ref "B1") (err "#LOOP!"),
      addF "B1" (ref "A1") (err "#LOOP!")]

\end{code}

\end{document}
