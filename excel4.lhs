\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}8
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%format @@ = "\oplus "
%format @@@ = "\otimes "
%format minR = "r_{min}"
%format maxR = "r_{max}"
%format minC = "c_{min}"
%format maxC = "c_{max}"
%format fromR = "r_{from}"
%format toR = "r_{to}"
%format fromC = "c_{from}"
%format toC = "c_{to}"
%format cellOp = "op_{cell}"
%format rowOp = "op_{row}"
%format rcFrom = "rc_{from}"
%format rcTo = "rc_{to}"
%format myRC = "rc_{root}"
%format accRow = "acc_{row}"
%format accCell = "acc_{cell}"
%format zeroRow = "zero_{row}"
%format valRow = "val_{row}"
%format maxRow = "r_{max}"
%format maxCol = "c_{max}"
%format (XlRC r c) = "\llparenthesis " r ", " c "\rrparenthesis "
%format (XlAbs v) = "\langle " v "\rangle "
%format (XlRel v) = "\langle " v "\rangle_{R}"

%format `Set.member` = "\in"
%format va = "v_a"
%format vb = "v_b"
%format vi = "v_i"
%format vr = "v_r"
%format na = "n_a"
%format nb = "n_b"
%format valuesa = "values_a"
%format valuesr = "values_r"
%format vstr = "v_{str}"
%format vstart = "v_{start}"
%format vlen = "v_{len}"
%format valuesacc = "values_{acc}"
%format valuesRow = "values_{row}"
%format valuesi = "values_{i}"
%format valuesm = "values_{m}"
%format vstr'
%format vstart'
%format vlen'
%format vsum = "v_{sum}"

\begin{document}

\title{An interpreter modelling spreadsheet semantics}
\author{Hisham Muhammad}

\maketitle{}

This is an interpreter designed to model the core semantics of spreadsheets,
with a focus on the dataflow language at its core. The intention here is to
describe the evaluation logic of the language, so the goal here is clarity,
not performance.

%This implementation uses only standard modules included in the Haskell Platform:

\begin{code}

import Data.Map.Strict as Map (  Map, empty, elems, mapWithKey, foldlWithKey, member,
                                 insert, lookup, toList, (!))
import Data.List (foldl')
import Data.Tree (flatten)
import Data.Char (ord, chr)
import Data.Set as Set (Set, insert, member, toList, singleton)
import Debug.Trace
import Data.Fixed
import Text.PrettyPrint.Boxes as Box (render, hcat, vcat, text)
import Text.PrettyPrint.Boxes as Alignment (left, right)
import ShowConcat ((@@), (@@@))

\end{code}

\section{Representation of programs}

\begin{code}

data XlWorksheet = XlWorksheet XlCells
   deriving Show

type XlCells = Map.Map XlRC XlCell

data XlRC = XlRC XlAddr XlAddr
   deriving (Eq, Ord)

data XlAddr  =  XlAbs  Int  -- absolute address
             |  XlRel  Int  -- relative address
   deriving (Eq, Ord)

data XlCell  =  XlCell XlFormula
             |  XlAFCell XlFormula (Int, Int)
   deriving Show


data XlFormula  =  XlLit  XlValue
                |  XlFun  String [XlFormula]
                |  XlRef  XlRC
                |  XlRng  XlRC XlRC
   deriving Show

data XlValue  =  XlNumber   Double
              |  XlString   String
              |  XlBoolean  Bool
              |  XlError    String
              |  XlMatrix   [[XlValue]]
   deriving Eq

\end{code}

\section{Representation of states}

\begin{code}

data XlState = XlState XlCells XlValues

type XlValues = Map.Map XlRC XlValue

\end{code}

Interaction with a spreadsheet consists exclusively of replacing formulas in
cells. We represent these as events that contain the absolute coordinates and
the formula to be entered to a cell. In the case of array formulas, a
rectangular range (denoted by the top-left and bottom-right cells) is given. A
single formula will then apply to that range as a group.

\begin{code}

data XlEvent  =  XlSetFormula       XlRC XlFormula
              |  XlSetArrayFormula  XlRC XlRC XlFormula
   deriving Show

\end{code}

\section{Execution}

The execution of a spreadsheet is demand-driven. The user triggers by editing cells,
which cause their value to be recomputed. When computing the value of a cell, other
cells may be referenced, so they are computed as well, and the process continues
recursively.

\subsection{Main loop}

Since we are interested in the dynamic semantics (that is, what happens with the 
program state over time as it runs), we model our interpreter as a loop of
evaluation steps. The main driver function takes a worksheet (a spreadsheet
document containing the initial values of cell formulas), a list of events,
and produces the final state, containing the resulting cells and their values.

\begin{code}
run :: XlWorksheet -> [XlEvent] -> XlState
run sheet@(XlWorksheet cells) events =
   foldl' runEvent (XlState cells Map.empty) events
\end{code}

For each event, we need to update the cells entered and then recalculate the
values of cells. Since we are not concerned with performance and formulas
are in principle purely functional, we simply recompute all cells in the
worksheet. This saves us from having to maintain a reverse dependency graph.
Still, in the auxiliary accumulator function |acc| we avoid recalculating
a cell if it was already calculated in this pass as a dependency of a previous
cell.

\begin{code}
runEvent :: XlState -> XlEvent -> XlState
runEvent env@(XlState cells _) event =
   let 
      cells' = updateCells cells event
      
      acc :: XlValues -> XlRC -> XlCell -> XlValues
      acc values rc cell =
         if Map.member rc values
         then values
         else snd $ calcCell (Set.singleton rc) cells' values rc cell
   in
      XlState cells' (Map.foldlWithKey acc Map.empty cells')
\end{code}

An event may update a single cell in case of a regular formula, or many cells
at a time in case of an array formula applied over a range.

\begin{code}
updateCells cells event@(XlSetFormula rc formula) =
   Map.insert rc (XlCell formula) cells

updateCells cells event@(XlSetArrayFormula rcFrom rcTo formula) =
   fst $ foldRange rcFrom rcFrom rcTo (cells, (0, 0)) id cellOp rowOp
      where
         cellOp (cells, (x, y)) rc  = (Map.insert rc (traceShowId $ XlAFCell formula (x, y)) cells, (x + 1, y))
         rowOp _ r (cells, (x, y))  = (cells, (0, y + 1))
\end{code}

To fold over a range, we use a custom folding function, which takes the base
coordinate and the addresses for the edges of the range, a zero-value for the
fold, an initializer function for each row, an accumulator function to run on
each cell, and another function to run as each row is completed.

\begin{code}
foldRange ::  XlRC -> XlRC -> XlRC     -- base coordinate and addresses for the range
              -> r                     -- a zero-value for the fold
              -> (r -> c)              -- an initializer function for each row
              -> (c -> XlRC -> c)      -- accumulator function to run on each cell
              -> (r -> Int -> c -> r)  -- function to run as each row completes
              -> r
foldRange rc from to zero zeroRow cellOp rowOp =
   let
      (minR, minC, maxR, maxC) = getRCs rc from to
      handleRow accRow r = rowOp accRow r valRow
         where
            handleCell accCell c = cellOp accCell (XlRC (XlAbs r) (XlAbs c))
            valRow = foldl' handleCell (zeroRow accRow) [minC..maxC]
   in
      foldl' handleRow zero [minR..maxR]
\end{code}

\section{Utility functions}

A few utility functions for addresses are used throughout the interpreter. We
will quickly introduce them here.

Function |getRCs| obtains the top-left and bottom-right absolute coordinates
of a range, given two opposite edge addresses and a base absolute address.

\begin{code}
getRCs :: XlRC -> XlRC -> XlRC -> (Int, Int, Int, Int)
getRCs myRC rcFrom rcTo =
   let
      XlRC (XlAbs fromR) (XlAbs fromC)  = toAbs myRC rcFrom
      XlRC (XlAbs toR)   (XlAbs toC)    = toAbs myRC rcTo
      minR  = min  fromR  toR
      maxR  = max  fromR  toR
      minC  = min  fromC  toC
      maxC  = max  fromC  toC
   in
      (minR, minC, maxR, maxC)
\end{code}

Function |toAbs| converts a relative address to an absolute one, based on a
base absolute address.

\begin{code}

toAbs :: XlRC -> XlRC -> XlRC
toAbs base@(XlRC br bc) cell@(XlRC cr cc) = XlRC (toAbsAddr br cr) (toAbsAddr bc cc)
   where 
      toAbsAddr :: XlAddr -> XlAddr -> XlAddr
      toAbsAddr  _               c@(XlAbs _)  = c
      toAbsAddr  (XlAbs b)       (XlRel c)    = XlAbs (b + c)
      toAbsAddr  b@(XlRel _)     _            = error ("base in toAbs must be absolute, got" @@@ b)

\end{code}

Function |toRC| converts Excel-style addresses in ``A1'' alphanumeric format
to the internal row-column numeric format. (For simplicity, we support only
rows A to Z.)

\begin{code}
toRC :: String -> XlRC
toRC (l:num) = XlRC (XlAbs ((read num) - 1)) (XlAbs ((ord l) - 65)) 
\end{code}

\section{Calculating cells}

To calculate the value of a cell we evaluate the cell's formula, potentially
recursing to evaluate other cells that formula. The |calcCell| takes as
arguments a set of cell addresses currently being recursively visited (to
detect cycles), the table of cell formulas, the current table of values, the
base address of the cell and the cell to compute. The function produces the
calculated value of the cell along with the map of all values, since other
calls may have been computed along the way). 

\begin{code}

calcCell :: Set XlRC -> XlCells -> XlValues -> XlRC -> XlCell -> (XlValue, XlValues)

\end{code}

A major complicator in the semantics of a spreadsheet application is the
fact that there are two distinct modes of evaluation: one for regular formulas,
and one for array formulas. Further, different kinds of functions in formulas
evaluate their arguments in different ways: borrowing from the terminology
of the language Perl, some functions evaluate their arguments in a scalar
context (that is, they expect their arguments to produce a scalar value),
and some evaluate arguments in an array context. This gives us four evaluation
functions in total.

To represent an evaluation mode, we define a data type |XlEvaluator| which,
besides carrying a few context values for convenience, includes a coercion
function |toScalar| to obtain a scalar function according to the context of a
cell (as we will see in more detail below), and two evaluation functions, one
for each of the possible evaluation contexts: |array| and |scalar|.

\begin{code}

data XlEvaluator = XlEvaluator {
   rc        :: XlRC,
   xy        :: (Int, Int),
   visiting  :: Set XlRC,
   cells     :: XlCells,
   toScalar  :: XlRC -> Int -> Int -> XlFormula -> XlFormula,
   array     :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues),
   scalar    :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
}

\end{code}

When calculating a regular formula, we define a ``simple'' evaluator and
trigger the scalar evaluation function on the formula.

\begin{code}

calcCell visiting cells values myRC@(XlRC (XlAbs r) (XlAbs c)) (XlCell formula) =
   (scalar ev) ev values formula
   where
      ev = XlEvaluator {
         rc = myRC,
         xy = (0, 0),
         cells = cells,
         visiting = visiting,
         toScalar = intersectScalar,
         scalar = simpleScalarEvaluator,
         array = simpleArrayEvaluator
      }

\end{code}

For calculating an array formula, we define a ``matrix'' evaluator. We also
trigger the calculation by applying this mode's scalar evaluator, but we
also filter the result value through a coercion function (|scalarize|),
since we want a scalar value to present in the cell.

\begin{code}

calcCell visiting cells values myRC (XlAFCell formula (x, y)) =
   scalarize ev $ (scalar ev) ev values formula
   where
      ev = XlEvaluator {
         rc = myRC,
         xy = (x, y),
         cells = cells,
         visiting = visiting,
         toScalar = matrixToScalar,
         scalar = matrixScalarEvaluator,
         array = matrixArrayEvaluator
      }

scalarize :: XlEvaluator -> (XlValue, XlValues) -> (XlValue, XlValues)
scalarize ev (value, values) = (value', values)
   where
      (x, y) = xy ev
      (XlLit value') = matrixToScalar (rc ev) x y (XlLit value)

\end{code}

\subsection{Simple cell evaluation}

\subsubsection{Scalar context evaluator}

\begin{code}

simpleScalarEvaluator ev values formula =
   let
      formula' = (toScalar ev) (rc ev) 0 0 formula
   in
      case formula' of
         XlLit v -> updateRC ev v values
         XlRef ref -> getRef ev values ref
         _ -> evalFormula ev values formula'

\end{code}

\subsubsection{Array context evaluator}

\begin{code}

simpleArrayEvaluator ev values formula =
   case formula of
      XlLit v -> updateRC ev v values
      XlRef ref -> getRef ev values ref
      _ -> evalFormula ev values formula

\end{code}

\subsubsection{Coercion to scalar}

\begin{code} 

-- 3.3) Non-Scalar Evaluation (aka 'Array expressions') [page 27]
-- 1) Evaluation as an implicit intersection of the argument with the expression's evaluation position.
intersectScalar :: XlRC -> Int -> Int -> XlFormula -> XlFormula
intersectScalar myRC@(XlRC (XlAbs r) (XlAbs c)) _ _ formula =
   case formula of
      XlLit (XlMatrix [])   -> XlLit (XlNumber 0) -- undocumented behavior of LibreOffice
      XlLit (XlMatrix [[]]) -> XlLit (XlNumber 0) -- undocumented behavior of LibreOffice
      XlLit (XlMatrix mtx)  -> XlLit (head (head mtx)) -- 1.1) Inline Arrays: Element (0;0) of the array is used in the place of the array.
      XlRng rcFrom rcTo ->
         let
            (fromR, fromC, toR, toC) = getRCs myRC rcFrom rcTo
         in
            if fromC == toC -- 1.2.1) If the target reference is a row-vector (Nx1) use the value at the intersection of the evaluation position's column and the reference's row.
            then
               if (r >= fromR) && (r <= toR)
               then XlRef (XlRC (XlAbs r) (XlAbs fromC))
               else XlLit (XlError "#VALUE!") -- Note 2: If there is no intersection the result is \texttt{\#VALUE!}
            else if fromR == toR -- 1.2.2) If the target reference is a column-vector (1xM) the value at the intersection of the evaluation position's row and the reference's column.
            then
               if (c >= fromC) && (c <= toC)
               then XlRef (XlRC (XlAbs fromR) (XlAbs c))
               else XlLit (XlError "#VALUE!")
            else
               XlLit (XlError "#VALUE!")
      f -> f

\end{code}

\subsection{Matrix cell evaluation}

\subsubsection{Scalar context evaluator}

\begin{code}

matrixScalarEvaluator ev values formula =
   let
      (x, y) = xy ev
      formula' = (toScalar ev) (rc ev) x y formula
   in
      trace ("RC " @@ (rc ev) @@ "\nMSE " @@ formula @@ "\n>>> " @@ formula' @@ "\n\n") $
      case formula' of
         XlLit v -> updateRC ev v values
         XlRef ref -> getRef ev values ref
         _ -> evalFormula ev values formula'

\end{code}

\subsubsection{Array context evaluator}

\begin{code}

matrixArrayEvaluator ev values formula =
   case formula of
      XlLit v -> updateRC ev v values
      XlRef ref -> getRef ev values ref
      _ -> iterateFormula ev values formula

\end{code}

Function |iterateFormula| takes a function where any argument is a range or a
matrix, and produces a matrix with results, evaluating the formula in scalar context
once for each cell, replacing the non-scalar argument with the scalar element
corresponding to the cell position. If the given function has no non-scalar
arguments, it is evaluated normally.

\begin{code}
iterateFormula :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
iterateFormula ev values (XlFun name args) =
   if maxX > 1 || maxY > 1
   then (\(m, v) -> (XlMatrix m, v)) $ foldl' doRow ([], values) [0..maxY-1]
   else evalFormula ev values (XlFun name args)
   where
      XlRC (XlAbs y) (XlAbs x) = rc ev
      maxY :: Int
      maxY = foldl getY 1 args
         where
            getY mx (XlLit (XlMatrix mtx))  = max mx (length mtx)
            getY mx (XlRng rcFrom rcTo)     = max mx (1 + toR - fromR)
               where
                  (fromR, fromC, toR, toC) = getRCs (rc ev) rcFrom rcTo
            getY mx _ = mx
      maxX :: Int
      maxX = foldl getX 1 args
         where
            getX mx (XlLit (XlMatrix mtx)) = max mx (maxRowLength mtx)
               where
                  maxRowLength :: [[XlValue]] -> Int
                  maxRowLength mtx = foldl (\mx' row -> max mx' (length row)) 1 mtx
            getX mx (XlRng rcFrom rcTo) = max mx (1 + toC - fromC)
               where
                  (fromR, fromC, toR, toC) = getRCs (rc ev) rcFrom rcTo
            getX mx _ = mx
      doRow :: ([[XlValue]], XlValues) -> Int -> ([[XlValue]], XlValues)
      doRow (mtx, values) y = appendTo mtx $ foldl doCell ([], values) [0..maxX-1]
         where
            doCell :: ([XlValue], XlValues) -> Int -> ([XlValue], XlValues)
            doCell (row, values) x = appendTo row
                                   $ evalFormula ev values (XlFun name (map ((toScalar ev) (rc ev) x y) args))
            appendTo :: [a] -> (a, b) -> ([a], b)
            appendTo xs (val, vals) = (xs ++ [val], vals)
\end{code}

\subsubsection{Coercion to scalar}

\begin{code} 

-- 2) Matrix evaluation
-- If an expression is being evaluated in a cell flagged as a being part of a 'Matrix'
-- (OpenDocument 8.1.3 table:number-matrix-columns-spanned):
matrixToScalar :: XlRC -> Int -> Int -> XlFormula -> XlFormula
matrixToScalar myRC x y formula =
   case formula of
      XlLit (XlMatrix mtx) ->
         displayRule x y (foldl' max 0 (map length mtx)) (length mtx) (\x y -> XlLit $ mtx !! y !! x)
      XlRng rcFrom rcTo ->
         trace ("Converted range (" @@ rcFrom @@@ rcTo @@ ") " @@ x @@@ y @@ " => ") $ traceShowId $ 
         displayRule x y (1 + toC - fromC) (1 + toR - fromR) (\x y -> trace ("FromR" @@@ fromR @@@ "x" @@@ x @@@ "FromC" @@@ fromC @@@ "y" @@@ y @@@ "=>") $ traceShowId $ XlRef (XlRC (XlAbs (fromR + y)) (XlAbs (fromC + x))))
            where 
               (fromR, fromC, toR, toC) = getRCs myRC rcFrom rcTo
      f -> f
   where
      -- 2.1) The portion of a non-scalar result to be displayed need not be co-extensive with a
      -- specified display area. The portion of the non-scalar result to be displayed is
      -- determined by:
      displayRule :: Int -> Int -> Int -> Int -> (Int -> Int -> XlFormula) -> XlFormula
      displayRule x y sizeX sizeY getXY =
         trace ("sizex" @@@ sizeX @@@ "sizey" @@@ sizeY) $
         -- 2.1.1) If the position to be displayed exists in the result, display that position.
         if sizeY > y && sizeX > x
         then trace ("got it") $ traceShowId $ getXY x y
         -- [Rules 2.1.2 and 2.1.3 apply to singletons]
         else if sizeX == 1 && sizeY == 1
         then getXY 0 0
         -- 2.1.2) If the non-scalar result is 1 column wide, subsequent columns in the display
         -- area display the value in the first column.
         else if sizeX == 1 && x > 0
         then getXY 0 y
         -- 2.1.3) If the non-scalar result is 1 row high, subsequent rows in the display area use
         -- the value of the first row.
         else if sizeY == 1 && y > 0
         then getXY x 0
         -- 2.1.4) If none of the other rules apply \texttt{\#N/A}
         else XlLit $ XlError "#N/A"

\end{code}

\section{Operations}

Function |toString| is a converter that presents rational and integer values
in their preferred notation (that is, with and without a decimal point,
respectively).

\begin{code}
toString :: Double -> String
toString n = if fromIntegral (floor n) /= n then show n else show (floor n)
\end{code}


\begin{code}

bool2num :: Bool -> Double
bool2num b = if b == True then 1 else 0

checkNumber :: (XlValue, XlValues) -> (XlValue, XlValues)
checkNumber (v, vs) = (coerce v, vs)
   where
      coerce (XlString s)     = case reads s :: [(Double, String)] of
                                       []       -> XlError "#VALUE!"
                                       [(n,_)]  -> XlNumber n
      coerce (XlBoolean b)    = XlNumber (bool2num b)
      coerce x                = x

checkString :: (XlValue, XlValues) -> (XlValue, XlValues)
checkString (v, vs) = (coerce v, vs)
   where
      coerce (XlNumber n)    = XlString (toString n)
      coerce (XlBoolean b)   = XlString (if b == True then "1" else "0")
      coerce x               = x

updateRC :: XlEvaluator -> XlValue -> XlValues -> (XlValue, XlValues)
updateRC ev v vs = (v, Map.insert (rc ev) v vs)

checkErr :: (XlValue -> XlValue -> XlValue) -> XlValue -> XlValue -> XlValue
checkErr op e@(XlError _)  _              = e
checkErr op _              e@(XlError _)  = e
checkErr op a              b              = op a b

binOp :: (Double -> Double -> Double) -> XlEvaluator -> XlValues -> XlFormula -> XlFormula -> (XlValue, XlValues)
binOp op ev values a b =
   let
      (va, values')   = checkNumber $ (scalar ev) ev values  a
      (vb, values'')  = checkNumber $ (scalar ev) ev values' b
      
      doOp (XlNumber na)  (XlNumber nb)   = XlNumber (op na nb)
      doOp _              _               = XlError "#VALUE!"
      
      val = checkErr doOp va vb
   in
      updateRC ev val values''

unOp :: (Double -> Double) -> XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
unOp op ev values v =
   let
      (v', values') = checkNumber $ (scalar ev) ev values v
      
      doOp e@(XlError _)  = e
      doOp (XlNumber n)   = XlNumber $ op n
      doOp _              = XlError "#VALUE!"
      
      val = doOp v'
   in
      updateRC ev val values'

evalFormula :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)

evalFormula ev values (XlRng from to) = 
   let 
      (mtx, valuesm) = foldRange (rc ev) from to ([], values) zeroRow cellOp rowOp
         where
            zeroRow :: ([[XlValue]], XlValues) -> ([XlValue], XlValues)
            zeroRow (_, vs) = ([], vs)
   
            cellOp :: ([XlValue], XlValues) -> XlRC -> ([XlValue], XlValues)
            cellOp (row, vs) rc =
               addToRow $ (scalar ev) ev vs (XlRef rc)
                  where addToRow (v, vs') = (row ++ [v], vs')
   
            rowOp :: ([[XlValue]], XlValues) -> Int -> ([XlValue], XlValues) -> ([[XlValue]], XlValues)
            rowOp (curMtx, _) r (row, vs) = (curMtx ++ [row], vs)
   in
      updateRC ev (XlMatrix mtx) valuesm

evalFormula ev values (XlFun "IF" [i, t, e]) =
   let
      (vi, valuesi) = checkNumber $ (scalar ev) ev values i
      (vr, valuesr) = 
         case vi of
            (XlError _)   -> (vi, valuesi)
            (XlNumber 0)  -> (scalar ev) ev valuesi e
            (XlNumber _)  -> (scalar ev) ev valuesi t
            _             -> ((XlError "#VALUE!"), valuesi)
   in
      updateRC ev vr valuesr

\end{code}

When parsing the string, we're missing error checking in |toRC|.

\begin{code}
evalFormula ev values (XlFun "INDIRECT" [addr]) =
   let
      convert s =
         case break (== ':') s of
            (a1, ':':b2)  -> (XlRng (toRC a1) (toRC b2))
            _             -> (XlRef (toRC s))

      (va, valuesa)  = checkString $ (scalar ev) ev values addr
      (vr, valuesr)  = 
         case va of
            XlError e   -> (va, valuesa)
            XlString s  -> (scalar ev) ev valuesa (convert s)
            _           -> ((XlError "#VALUE!"), valuesa)
   in
      updateRC ev vr valuesr

evalFormula ev values (XlFun "MID" [vstr, vstart, vlen]) =
   let
      (vstr',    values')    = checkString  $ (scalar ev) ev values    vstr
      (vstart',  values'')   = checkNumber  $ (scalar ev) ev values'   vstart
      (vlen',    values''')  = checkNumber  $ (scalar ev) ev values''  vlen
      
      doMid (XlString str) (XlNumber start) (XlNumber len) =
         XlString $ take (floor len) $ drop (floor start - 1) str
      doMid _ _ _ = XlError "#VALUE!"
      
      val = doMid vstr' vstart' vlen'
   in
      updateRC ev val values'''

evalFormula ev values (XlFun "SQRT"  [v])  = unOp sqrt  ev values v
evalFormula ev values (XlFun "ABS"   [v])  = unOp abs   ev values v

evalFormula ev values (XlFun "SUM" inputs) =
   let
      
      doSum  s@(XlString _)  v               = v
      doSum  v               s@(XlString _)  = v
      doSum  (XlBoolean b)   (XlNumber n)    = XlNumber (bool2num b + n)
      doSum  (XlNumber n)    (XlBoolean b)   = XlNumber (bool2num b + n)
      doSum  (XlNumber a)    (XlNumber b)    = XlNumber (a + b)
      
      (vr, valuesr) = foldl' handle (XlNumber 0, values) inputs
         where
            handle (acc, valuesacc) input =
               let
                  (vi, valuesi) = (array ev) ev valuesacc input
                  vsum =
                     case vi of
                        XlError _     -> vi
                        XlMatrix mtx  -> foldl' (foldl' (checkErr doSum)) acc mtx
                        XlBoolean b   -> checkErr doSum acc vi
                        XlNumber n    -> checkErr doSum acc vi
                        _             -> XlError "#VALUE!"
               in
                  (vsum, valuesi)
   in
      updateRC ev vr valuesr

evalFormula ev values (XlFun "+" [a, b])  = binOp (+)  ev values a b
evalFormula ev values (XlFun "*" [a, b])  = binOp (*)  ev values a b

evalFormula ev values (XlFun "&" [a, b]) =
   let
      (va, values')   = checkString $ (scalar ev) ev values   a
      (vb, values'')  = checkString $ (scalar ev) ev values'  b
      
      doConcat  (XlString sa)  (XlString sb)   = XlString (sa ++ sb)
      doConcat  _              _               = XlError "#VALUE!"
      
      val = checkErr doConcat va vb
   in
      updateRC ev val values''

evalFormula ev values (XlFun "/" [a, b]) =
   let
      (va,  values')   = checkNumber $ (scalar ev) ev values   a
      (vb,  values'')  = checkNumber $ (scalar ev) ev values'  b
      
      doDiv  (XlNumber na)  (XlNumber 0)   = XlError "#DIV/0!"
      doDiv  (XlNumber na)  (XlNumber nb)  = XlNumber (na / nb)
      doDiv  _              _              = XlError "#VALUE!"
      
      val = checkErr doDiv va vb
   in
      updateRC ev val values''

\end{code}

In case of two errors, the first one is propagated.

\begin{code}
evalFormula ev values (XlFun "=" [a, b]) =
   let
      (va,  values')  = (scalar ev) ev values  a
      (vb,  values'') = (scalar ev) ev values' b
      
      doEq  (XlNumber na)   (XlNumber nb)   = XlBoolean (na == nb)
      doEq  (XlString sa)   (XlString sb)   = XlBoolean (sa == sb)
      doEq  (XlBoolean ba)  (XlBoolean bb)  = XlBoolean (ba == bb)
      doEq  (XlNumber na)   (XlBoolean bb)  = XlBoolean (na == bool2num bb)
      doEq  (XlBoolean ba)  (XlNumber nb)   = XlBoolean (bool2num ba == nb)
      doEq  _               _               = XlBoolean False
      
      val = checkErr doEq va vb
   in
      updateRC ev val values''

evalFormula ev values (XlFun _ _) = updateRC ev (XlError "#NAME?") values

getRef :: XlEvaluator -> XlValues -> XlRC -> (XlValue, XlValues)
getRef ev values ref' =
   let
      ref = toAbs (rc ev) ref'
      give val = (val, Map.insert (rc ev) val values)
   in
      if ref `Set.member` (visiting ev)
      then give (XlError "#LOOP!")
      else 
         case Map.lookup ref values of
            Just v   -> give v
            Nothing  ->
               case Map.lookup ref (cells ev) of
                  Just cell  -> give $ fst $ calcCell (Set.insert ref (visiting ev)) (cells ev) values ref cell
                  Nothing    -> give (XlNumber 0)

\end{code}

\section{Demonstration}

In order to produce a more readable output, we define the instance |Show| for
our data types using the |Text.PrettyPrint| package to produce tabular
outputs.

\begin{code}

instance Show XlValue where
   show (XlNumber d)  = toString d
   show (XlString s)  = show s
   show (XlBoolean b) = show b
   show (XlError e)   = show e
   show (XlMatrix m)  = show m

instance Show XlAddr where
   show (XlAbs n) = show n
   show (XlRel n) = "[" @@ n @@ "]"

instance Show XlRC where
   show (XlRC r@(XlAbs rn) c@(XlAbs cn)) = "<" @@ [chr (cn + 65)] @@ (rn + 1) @@ ">"
   show (XlRC r c) = "R" @@ r @@ "C" @@ c

instance Show XlState where
   show (XlState cells values) = 
      "\nCells:\n" @@
      listCells @@
      "\nValues:\n" @@
      tableValues @@
      "\n" @@
      values @@
      "\n"
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
                            $ Box.text ['|', chr (c + 65)] : map (\s -> Box.text ('|' : doRow s)) [0..maxRow]
               where
                  doRow r =
                     let
                        rc = (XlRC (XlAbs r) (XlAbs c))
                        val = Map.lookup rc values
                        lpad m xs = reverse $ take m $ reverse $ (take m $ repeat ' ') ++ (take m xs)
                     in
                        case val of
                           Just (XlNumber n)  -> lpad 11 (toString n)
                           Just v             -> show v
                           Nothing            -> ""

\end{code}

Here, we run a suite of tests, checking for results described in the specification documents.

We employ a few shortcuts to write down formulas:

\begin{code}
str :: String -> XlFormula
str s = XlLit (XlString s)

num :: Double -> XlFormula
num n = XlLit (XlNumber n)

nummtx :: [[Double]] -> XlFormula
nummtx mx = XlLit (XlMatrix (map (map XlNumber) mx))

ref :: String -> XlFormula
ref a1 = XlRef (toRC a1)

range :: String -> String -> XlFormula
range a1 b2 = XlRng (toRC a1) (toRC b2)
\end{code}

Then, we construct a test driver function that runs test cases and compares the results to expected values.

\begin{code}
runTest :: String -> [(XlEvent, XlValue)] -> IO ()
runTest name operations =
   let
      env@(XlState cells values) = run (XlWorksheet Map.empty) (map fst operations)
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
      print name
      print env
      if null failures
      then putStrLn "OK! :-D"
      else
         do
            putStrLn "****************** Failed: ******************"
            print failures
\end{code}

Finally, we run the tests.

\begin{code}

main :: IO ()
main = 
      do
         runTest "Example" [
            ( XlSetFormula (toRC "A1") (num 15),                                      XlNumber 15 ),
            ( XlSetFormula (toRC "B1") (num 0),                                       XlNumber 15 ),
            ( XlSetFormula (toRC "A2") (XlFun "+" [ref "A1", ref "B1"]),              XlNumber 30 ),
            ( XlSetFormula (toRC "B1") (ref "A1"),                                    XlNumber 15 ),
            ( XlSetFormula (toRC "C1") (str "B"),                                     XlString "B" ),
            ( XlSetFormula (toRC "C2") (num 1),                                       XlNumber 1 ),
            ( XlSetFormula (toRC "B2") (XlFun "INDIRECT" [XlFun "&" [ref "C1", ref "C2"]]), XlNumber 15 ),
            ( XlSetFormula (toRC "D1") (XlFun "SUM" [range "A1" "B2"]),              XlNumber 75 ),
            ( XlSetFormula (toRC "E1") (XlFun "SUM" [range "B1" "B2"]),              XlNumber 30 ),
            ( XlSetFormula (toRC "F1") (XlFun "SUM" [range "D1" "E1"]),              XlNumber 105 ),
            
            ( XlSetFormula (toRC "H1") (num 10),                                     XlNumber 10 ),
            ( XlSetFormula (toRC "I1") (num (-20)),                                  XlNumber (-20) ),
            ( XlSetFormula (toRC "J1") (num 30),                                     XlNumber 30 ),
            ( XlSetFormula (toRC "I2") (XlFun "ABS" [range "H1" "J1"]),              XlNumber 20 ),
            ( XlSetFormula (toRC "K2") (XlFun "ABS" [range "H1" "J1"]),              XlError "#VALUE!" ),
            
            ( XlSetFormula (toRC "A10") (num 10),                                    XlNumber 10 ),
            ( XlSetFormula (toRC "A11") (str "10"),                                  XlString "10" ),
            ( XlSetFormula (toRC "A12") (XlFun "=" [ref "A10", ref "A11"]),          XlBoolean False ),
            ( XlSetFormula (toRC "A13") (XlFun "=" [ref "A10", num 10]),             XlBoolean True ),
            ( XlSetFormula (toRC "A14") (XlFun "=" [ref "A13", num 1]),              XlBoolean True ),
   
            ( XlSetFormula (toRC "A15") (XlFun "/" [num 1, num 0]),                  XlError "#DIV/0!" ),
            ( XlSetFormula (toRC "A16") (XlFun "=" [ref "K2", ref "A15"]),           XlError "#VALUE!" ),
            ( XlSetFormula (toRC "A17") (XlFun "=" [ref "A15", ref "K2"]),           XlError "#DIV/0!" ),

            ( XlSetFormula (toRC "G1") (XlFun "+" [num 1000, range "A1" "A2"]),      XlNumber 1015 ),

            ( XlSetFormula (toRC "C5") (range "A1" "A2"),                            XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "F5") (toRC "F6") (nummtx [[15], [16]]), XlNumber 15 ),
            ( XlSetArrayFormula (toRC "D5") (toRC "D6") (XlFun "+" [range "A1" "A2", num 100]), XlNumber 115 )
            ]
         runTest "3.3 1 1.1) Note 1" [
            ( XlSetFormula (toRC "A1") (XlFun "ABS" [nummtx [[-3, -4]]]), XlNumber 3 ),
            ( XlSetFormula (toRC "A2") (XlFun "ABS" [nummtx [[-3], [-4]]]), XlNumber 3 ),
            ( XlSetFormula (toRC "A3") (XlFun "ABS" [nummtx [[-3, -4], [-6, -8]]]), XlNumber 3 ),
            ( XlSetFormula (toRC "A4") (nummtx [[1, 2, 3], [4, 5, 6]]), XlNumber 1 )
            ]
         runTest "3.3 1 1.2 1.2.1) Notes 2 and 3" [
            ( XlSetFormula (toRC "A1") (num (-10)), XlNumber (-10) ),
            ( XlSetFormula (toRC "B1") (num (-20)), XlNumber (-20) ),
            ( XlSetFormula (toRC "C1") (num (-30)), XlNumber (-30) ),
            ( XlSetFormula (toRC "B2") (XlFun "ABS" [range "A1" "C1"]), XlNumber 20 ),
            ( XlSetFormula (toRC "D4") (XlFun "ABS" [range "A1" "C1"]), XlError "#VALUE!" )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.1" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B3") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlSetFormula (toRC "C3") (ref "B2"), XlNumber 4 )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.2" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B3") (nummtx [[1],[3],[5]]), XlNumber 1 ),
            ( XlSetFormula (toRC "C3") (ref "B2"), XlNumber 3 )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.3" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B3") (nummtx [[2,4]]), XlNumber 2 ),
            ( XlSetFormula (toRC "C3") (XlRef (toRC "B2")), XlNumber 4 )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.3" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C4") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlSetFormula (toRC "D1") (XlRef (toRC "C1")), XlError "#N/A" ),
            ( XlSetFormula (toRC "D2") (XlRef (toRC "A4")), XlError "#N/A" )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 6" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlSetFormula (toRC "D1") (XlRef (toRC "B3")), XlNumber 0 )
            ]
         runTest "3.3 2 2.2 2.2.1) Note 7 (oasis_note7.ods)" [
            ( XlSetFormula      (toRC "A1")             (num 10), XlNumber 10 ),
            ( XlSetFormula      (toRC "A2")             (num 20), XlNumber 20 ),

            ( XlSetFormula      (toRC "B3")             (XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]), XlNumber 10 ),
            ( XlSetArrayFormula (toRC "B4") (toRC "C4") (XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]), XlNumber 10 ),
            ( XlSetFormula      (toRC "B5")             (XlFun "SUM" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]), XlNumber 10 ),
            ( XlSetArrayFormula (toRC "B6") (toRC "B6") (XlFun "SUM" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]), XlNumber 30 ),
            ( XlSetFormula      (toRC "B7")             (XlFun "SUM" [XlFun "SQRT" [range "A1" "A2"]]), XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "B8") (toRC "B8") (XlFun "SUM" [XlFun "SQRT" [range "A1" "A2"]]), XlNumber (sqrt 10 + sqrt 20) ),
            ( XlSetFormula      (toRC "C7")             (XlFun "SQRT" [range "A1" "A2"]), XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "C8") (toRC "C9") (XlFun "SQRT" [range "A1" "A2"]), XlNumber (sqrt 10) ),

            ( XlSetFormula      (toRC "D3")             (nummtx [[10, 20]]), XlNumber 10 ),
            ( XlSetArrayFormula (toRC "D4") (toRC "E4") (nummtx [[10, 20]]), XlNumber 10 ),
            ( XlSetFormula      (toRC "D5")             (XlFun "SUM" [nummtx [[10,20]]]), XlNumber 30 ),
            ( XlSetArrayFormula (toRC "D6") (toRC "D6") (XlFun "SUM" [nummtx [[10,20]]]), XlNumber 30 ),
            ( XlSetFormula      (toRC "D7")             (XlFun "SUM" [XlFun "SQRT" [nummtx [[10,20]]]]), XlNumber (sqrt 10) ),
            ( XlSetArrayFormula (toRC "D8") (toRC "D8") (XlFun "SUM" [XlFun "SQRT" [nummtx [[10,20]]]]), XlNumber (sqrt 10 + sqrt 20) ),
            ( XlSetFormula      (toRC "E7")             (XlFun "SQRT" [nummtx [[10,20]]]), XlNumber (sqrt 10) ),
            ( XlSetArrayFormula (toRC "E8") (toRC "E9") (XlFun "SQRT" [nummtx [[10],[20]]]), XlNumber (sqrt 10) )
            ]
         runTest "3.3 2 2.2 2.2.1) Note 8.1" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C1") (XlFun "+" [nummtx [[1,2]], nummtx [[3,4,5]]]), XlNumber 4 ),
            ( XlSetFormula (toRC "A2") (ref "A1"), XlNumber 4 ),
            ( XlSetFormula (toRC "B2") (ref "B1"), XlNumber 6 ),
            ( XlSetFormula (toRC "C2") (ref "C1"), XlError "#N/A" )
            ]
         runTest "3.3 2 2.2 2.2.1) Note 8.2" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B1") (XlFun "+" [nummtx [[1]], nummtx [[1,2]]]), XlNumber 2 ),
            ( XlSetFormula (toRC "A2") (ref "A1"), XlNumber 2 ),
            ( XlSetFormula (toRC "B2") (ref "B1"), XlNumber 3 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 9.1" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C2") (XlFun "+" [num 1, nummtx [[1,2,3],[4,5,6]]]), XlNumber 2 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 2 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 3 ),
            ( XlSetFormula (toRC "F1") (ref "C1"), XlNumber 4 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 5 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 6 ),
            ( XlSetFormula (toRC "F2") (ref "C2"), XlNumber 7 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 9.2" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C2") (XlFun "+" [nummtx [[1]], nummtx [[1,2,3],[4,5,6]]]), XlNumber 2 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 2 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 3 ),
            ( XlSetFormula (toRC "F1") (ref "C1"), XlNumber 4 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 5 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 6 ),
            ( XlSetFormula (toRC "F2") (ref "C2"), XlNumber 7 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 10" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1],[2]], nummtx [[10,20],[30,40]]]), XlNumber 11 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 21 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 32 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 42 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 11" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1,2]], nummtx [[10,20],[30,40]]]), XlNumber 11 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 22 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 31 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 42 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 12" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1,2]], nummtx [[10],[20]]]), XlNumber 11 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 12 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 21 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 22 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 13" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C1") (XlFun "MID" [str "abcd", nummtx [[1,2]], nummtx [[1,2,3]]]), XlString "a" ),
            ( XlSetFormula (toRC "A2") (ref "A1"), XlString "a" ),
            ( XlSetFormula (toRC "B2") (ref "B1"), XlString "bc" ),
            ( XlSetFormula (toRC "C2") (ref "C1"), XlError "#VALUE!" )
            ]

         runTest "ISO/IEC 29500:1 2012 page 2040 ex.1" [
            ( XlSetFormula (toRC "B2") (num 1), XlNumber 1),
            ( XlSetFormula (toRC "B3") (num 2), XlNumber 2),
            ( XlSetFormula (toRC "B4") (num 3), XlNumber 3),
            ( XlSetFormula (toRC "C2") (num 4), XlNumber 4),
            ( XlSetFormula (toRC "C3") (num 5), XlNumber 5),
            ( XlSetFormula (toRC "C4") (num 6), XlNumber 6),
            ( XlSetArrayFormula (toRC "D2") (toRC "D4") (XlFun "+" [XlFun "*" [range "B2" "B4", range "C2" "C4"], num 10.5]), XlNumber 14.5),
            ( XlSetFormula (toRC "E2") (ref "D2"), XlNumber 14.5 ),
            ( XlSetFormula (toRC "E3") (ref "D3"), XlNumber 20.5 ),
            ( XlSetFormula (toRC "E4") (ref "D4"), XlNumber 28.5 )
            ]

         runTest "ISO/IEC 29500:1 2012 page 2040 exs.2 and 3" [
            ( XlSetFormula (toRC "A1") (XlFun "SQRT" [nummtx [[1,2,3,4]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "B1") (toRC "B1") (XlFun "SQRT" [nummtx [[1,2,3,4]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "C1") (toRC "G1") (XlFun "SQRT" [nummtx [[1,2,3,4]]]), XlNumber 1 ),
            ( XlSetFormula (toRC "C2") (ref "C1"), XlNumber (sqrt 1)),
            ( XlSetFormula (toRC "D2") (ref "D1"), XlNumber (sqrt 2)),
            ( XlSetFormula (toRC "E2") (ref "E1"), XlNumber (sqrt 3)),
            ( XlSetFormula (toRC "F2") (ref "F1"), XlNumber (sqrt 4)),
            ( XlSetFormula (toRC "G2") (ref "G1"), XlError "#N/A")
            ]

         runTest "ISO/IEC 29500:1 2012 page 2040 ex.4 ***** consistent with LibreOffice, inconsistent with ISO doc" [
            ( XlSetFormula      (toRC "A1")             (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "A2") (toRC "A2") (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber ((sqrt 1)+(sqrt 2)+(sqrt 3)+(sqrt 4)) )
            ]
         runTest "more tests" [
            ( XlSetFormula      (toRC "A1")             (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "A2") (toRC "A2") (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber ((sqrt 1)+(sqrt 2)+(sqrt 3)+(sqrt 4)) ),
            ( XlSetArrayFormula (toRC "B1") (toRC "E1") (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber ((sqrt 1)+(sqrt 2)+(sqrt 3)+(sqrt 4)) ),
            ( XlSetArrayFormula (toRC "A3") (toRC "D3") (XlFun "SUM" [XlFun "+" [nummtx [[1,2,3,4]], num 100]]), XlNumber 410 )
            ]

         runTest "more tests" [
            ( XlSetFormula      (toRC "A1")             (num 1), XlNumber 1 ),
            ( XlSetFormula      (toRC "A2")             (num 2), XlNumber 2 ),
            ( XlSetFormula      (toRC "A3")             (num 3), XlNumber 3 ),
            ( XlSetFormula      (toRC "A4")             (num 4), XlNumber 4 ),
            ( XlSetFormula      (toRC "B5")             (XlFun "SUM" [XlFun "+" [range "A1" "A4", num 100]]), XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "B6") (toRC "B6") (XlFun "SUM" [XlFun "+" [range "A1" "A4", num 100]]), XlNumber 410 )
            ]
         
         runTest "more tests" [
            ( XlSetFormula      (toRC "A3") (str "F"), XlString "F" ),
            ( XlSetFormula      (toRC "B3")             (XlFun "SUM" [XlFun "+" [nummtx [[1,2,3,4]], num 100]]), XlNumber 410 ),
            ( XlSetFormula      (toRC "A4") (str "AF"), XlString "AF" ),
            ( XlSetArrayFormula (toRC "B4") (toRC "B4") (XlFun "SUM" [XlFun "+" [nummtx [[1,2,3,4]], num 100]]), XlNumber 410 ),
            ( XlSetFormula      (toRC "A5") (str "F"), XlString "F" ),
            ( XlSetFormula      (toRC "B5")             (XlFun "SUM" [XlFun "+" [XlFun "ABS" [nummtx [[1,2,3,4]] ], num 100]]), XlNumber 101 ),
            ( XlSetFormula      (toRC "A6") (str "AF"), XlString "AF" ),
            ( XlSetArrayFormula (toRC "B6") (toRC "B6") (XlFun "SUM" [XlFun "+" [XlFun "ABS" [nummtx [[1,2,3,4]] ], num 100]]), XlNumber 410 )
            ]

\end{code}



%\begin{code}
%
%main :: IO ()
%main = 
%      do
%         runTest "Formula evaluation incompatibilities" [
%            ( XlSetFormula       (toRC "A1")                (num 10), XlNumber 10 ),
%            ( XlSetFormula       (toRC "A2")                (num 20), XlNumber 20 ),
%            ( XlSetFormula       (toRC "B1")                (num 20), XlNumber 20 ),
% 
%            ( XlSetFormula       (toRC "D3")                (XlFun "SUM" [XlFun "SQRT" [nummtx [[10,20]]]]), XlNumber (sqrt 10) ),
%            ( XlSetArrayFormula  (toRC "D4") (toRC "D4")    (XlFun "SUM" [XlFun "SQRT" [nummtx [[10,20]]]]), XlNumber (sqrt 10 + sqrt 20) ),
%            ( XlSetFormula       (toRC "D5")                (XlFun "SUM" [XlFun "SQRT" [range "A1" "A2"]]), XlError "#VALUE!" ),
%            ( XlSetArrayFormula  (toRC "D6") (toRC "D6")    (XlFun "SUM" [XlFun "SQRT" [range "A1" "A2"]]), XlNumber (sqrt 10 + sqrt 20) ),
%            ( XlSetFormula       (toRC "D7")                (XlFun "SUM" [XlFun "SQRT" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]]), XlNumber (sqrt 10) ),
%            ( XlSetArrayFormula  (toRC "D8") (toRC "D8")    (XlFun "SUM" [XlFun "SQRT" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]]), XlNumber (sqrt 10 + sqrt 20) ),
%            ( XlSetFormula       (toRC "D9")                (XlFun "SUM" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]), XlNumber 10 ),
%            ( XlSetArrayFormula  (toRC "D10") (toRC "D10")  (XlFun "SUM" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]), XlNumber 30 ),
%
%            ( XlSetFormula       (toRC "D11")               (XlFun "SQRT" [nummtx [[10,20]]]), XlNumber (sqrt 10) ),
%            ( XlSetArrayFormula  (toRC "D12") (toRC "E12")  (XlFun "SQRT" [nummtx [[10,20]]]), XlNumber (sqrt 10) ),
%            ( XlSetFormula       (toRC "D13")               (XlFun "SQRT" [range "A1" "B1"]), XlError "#VALUE!" ),
%            ( XlSetArrayFormula  (toRC "D14") (toRC "E14")  (XlFun "SQRT" [range "A1" "B1"]), XlNumber (sqrt 10) ),
%            ( XlSetFormula       (toRC "D15")               (XlFun "SQRT" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "B1"]])]]), XlNumber (sqrt 10) ),
%            ( XlSetArrayFormula  (toRC "D16") (toRC "E16")  (XlFun "SQRT" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "B1"]])]]), XlNumber (sqrt 10) ),
%            ( XlSetFormula       (toRC "D17")               (XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "B1"]])]), XlNumber 10 ),
%            ( XlSetArrayFormula  (toRC "D18") (toRC "E18")  (XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "B1"]])]), XlNumber 10 )]
%
%         runTest "Circular references" [
%            ( XlSetFormula      (toRC "B1")             (num 100),  XlError "#LOOP!" ),
%            ( XlSetFormula      (toRC "A1")             (ref "B1"), XlError "#LOOP!" ),
%            ( XlSetFormula      (toRC "B1")             (ref "A1"), XlError "#LOOP!" )]
%
%\end{code}
%
\end{document}
