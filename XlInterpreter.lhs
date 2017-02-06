\documentclass[a4paper]{article}
\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%include Xl_format.lhs

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

module XlInterpreter where

import Data.Char (ord, chr)
import Data.Fixed
import Data.List (foldl')
import Data.Map.Strict as Map (Map, foldlWithKey, member, insert, empty, lookup)
import Data.Set as Set (Set, insert, member, singleton)

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

For convenience we define a few instances of |Show| that will prove useful
later when running the interpreter.

\begin{code}
instance Show XlRC where
   show (XlRC r@(XlAbs rn) c@(XlAbs cn)) =
      "<" ++ [chr (cn + 65)] ++ (show (rn + 1)) ++ ">"
   show (XlRC r c) =
      "R" ++ show r ++ "C" ++ show c

instance Show XlValue where
   show (XlNumber d)  = num2str d
   show (XlString s)  = show s
   show (XlBoolean b) = show b
   show (XlError e)   = show e
   show (XlMatrix m)  = show m

instance Show XlAddr where
   show (XlAbs n) = show n
   show (XlRel n) = "[" ++ show n ++ "]"
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
runEvents :: XlWorksheet -> [XlEvent] -> XlState
runEvents sheet@(XlWorksheet cells) events =
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
         cellOp (cells, (x, y)) rc  = (Map.insert rc (XlAFCell formula (x, y)) cells, (x + 1, y))
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
      toAbsAddr  b@(XlRel _)     _            = error ("base in toAbs must be absolute, got " ++ show b)

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
         displayRule x y (1 + toC - fromC) (1 + toR - fromR)
                     (\x y ->
                        XlRef (XlRC (XlAbs (fromR + y)) (XlAbs (fromC + x))))
            where 
               (fromR, fromC, toR, toC) = getRCs myRC rcFrom rcTo
      f -> f
   where
      -- 2.1) The portion of a non-scalar result to be displayed need not be co-extensive with a
      -- specified display area. The portion of the non-scalar result to be displayed is
      -- determined by:
      displayRule :: Int -> Int -> Int -> Int -> (Int -> Int -> XlFormula) -> XlFormula
      displayRule x y sizeX sizeY getXY =
         -- 2.1.1) If the position to be displayed exists in the result, display that position.
         if sizeY > y && sizeX > x
         then getXY x y
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

FIXME remove

\begin{code}
updateRC :: XlEvaluator -> XlValue -> XlValues -> (XlValue, XlValues)
updateRC ev v vs = (v, Map.insert (rc ev) v vs)
\end{code}

\begin{code}
getRef :: XlEvaluator -> XlValues -> XlRC -> (XlValue, XlValues)
getRef ev values ref' =
   let
      ref = toAbs (rc ev) ref'
      ret val = (val, Map.insert (rc ev) val values)
   in
      if ref `Set.member` (visiting ev)
      then ret (XlError "#LOOP!")
      else 
         case Map.lookup ref values of
            Just v   -> ret v
            Nothing  ->
               case Map.lookup ref (cells ev) of
                  Just cell  -> ret $ fst $ calcCell (Set.insert ref (visiting ev)) (cells ev) values ref cell
                  Nothing    -> ret (XlNumber 0)
\end{code}

\begin{code}
evalFormula :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
\end{code}

\subsubsection{Ranges}

\begin{code}
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
\end{code}

\subsubsection{\texttt{IF}}

\begin{code}
evalFormula ev values (XlFun "IF" [i, t, e]) =
   let
      (vi, valuesi) = toNumber $ (scalar ev) ev values i
      (vr, valuesr) = 
         case vi of
            (XlError _)   -> (vi, valuesi)
            (XlNumber 0)  -> (scalar ev) ev valuesi e
            (XlNumber _)  -> (scalar ev) ev valuesi t
            _             -> ((XlError "#VALUE!"), valuesi)
   in
      updateRC ev vr valuesr

\end{code}

\subsubsection{\texttt{INDIRECT}}

When parsing the string, we're missing error checking in |toRC|.

\begin{code}
evalFormula ev values (XlFun "INDIRECT" [addr]) =
   let
      convert s =
         case break (== ':') s of
            (a1, ':':b2)  -> (XlRng (toRC a1) (toRC b2))
            _             -> (XlRef (toRC s))

      (va, valuesa)  = toString $ (scalar ev) ev values addr
      (vr, valuesr)  = 
         case va of
            XlError e   -> (va, valuesa)
            XlString s  -> (scalar ev) ev valuesa (convert s)
            _           -> ((XlError "#VALUE!"), valuesa)
   in
      updateRC ev vr valuesr
\end{code}

\subsubsection{\texttt{MID}}

\begin{code}
evalFormula ev values (XlFun "MID" [vstr, vstart, vlen]) =
   let
      (vstr',    values')    = toString  $ (scalar ev) ev values    vstr
      (vstart',  values'')   = toNumber  $ (scalar ev) ev values'   vstart
      (vlen',    values''')  = toNumber  $ (scalar ev) ev values''  vlen
      
      doMid (XlString str) (XlNumber start) (XlNumber len) =
         XlString $ take (floor len) $ drop (floor start - 1) str
      doMid _ _ _ = XlError "#VALUE!"
      
      val = doMid vstr' vstart' vlen'
   in
      updateRC ev val values'''
\end{code}

\subsubsection{\texttt{SUM}}

\begin{code}
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
\end{code}

\subsubsection{Numeric operators}

\begin{code}

evalFormula ev values (XlFun "SQRT"  [v])  = unOp   sqrt  ev values v
evalFormula ev values (XlFun "ABS"   [v])  = unOp   abs   ev values v
evalFormula ev values (XlFun "+" [a, b])   = binOp  (+)   ev values a b
evalFormula ev values (XlFun "*" [a, b])   = binOp  (*)   ev values a b

evalFormula ev values (XlFun "&" [a, b]) =
   let
      (va, values')   = toString $ (scalar ev) ev values   a
      (vb, values'')  = toString $ (scalar ev) ev values'  b
      
      doConcat  (XlString sa)  (XlString sb)   = XlString (sa ++ sb)
      doConcat  _              _               = XlError "#VALUE!"
      
      val = checkErr doConcat va vb
   in
      updateRC ev val values''

evalFormula ev values (XlFun "/" [a, b]) =
   let
      (va,  values')   = toNumber $ (scalar ev) ev values   a
      (vb,  values'')  = toNumber $ (scalar ev) ev values'  b
      
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

\end{code}

\subsection{Type conversions}

Function |num2str| is a converter that presents rational and integer values
in their preferred notation (that is, with and without a decimal point,
respectively). Function |bool2num| converts booleans to 0 or 1.

\begin{code}
num2str :: Double -> String
num2str n = if fromIntegral (floor n) /= n then show n else show (floor n)

bool2num :: Bool -> Double
bool2num b = if b == True then 1 else 0
\end{code}

Functions |toNumber| and |toString| attempt to convert a value to the
specified type, producing a |XlError| value if its is not convertible.

\begin{code}
toNumber :: (XlValue, XlValues) -> (XlValue, XlValues)
toNumber (v, vs) = (coerce v, vs)
   where
      coerce (XlString s)     = case reads s :: [(Double, String)] of
                                       []       -> XlError "#VALUE!"
                                       [(n,_)]  -> XlNumber n
      coerce (XlBoolean b)    = XlNumber (bool2num b)
      coerce (XlMatrix _)     = XlError "#VALUE!"
      coerce v                = v

toString :: (XlValue, XlValues) -> (XlValue, XlValues)
toString (v, vs) = (coerce v, vs)
   where
      coerce (XlNumber n)    = XlString (num2str n)
      coerce (XlBoolean b)   = XlString (if b == True then "1" else "0")
      coerce (XlMatrix _)    = XlError "#VALUE!"
      coerce v               = v
\end{code}

Function |checkErr| checks input values for errors before performing
a binary operation. The order errors are evaluated is relevant: if
the first argument contains an error, it takes precedence.

\begin{code}
checkErr :: (XlValue -> XlValue -> XlValue) -> XlValue -> XlValue -> XlValue
checkErr op e@(XlError _)  _              = e
checkErr op _              e@(XlError _)  = e
checkErr op a              b              = op a b
\end{code}

These are convenience functions that encapsulate the pattern for common
unary and binary numeric functions.

\begin{code}
binOp ::  (Double -> Double -> Double)
          -> XlEvaluator -> XlValues -> XlFormula -> XlFormula -> (XlValue, XlValues)
binOp op ev values a b =
   let
      (va, values')   = toNumber $ (scalar ev) ev values  a
      (vb, values'')  = toNumber $ (scalar ev) ev values' b
      
      doOp (XlNumber na)  (XlNumber nb)   = XlNumber (op na nb)
      doOp _              _               = XlError "#VALUE!"
      
      val = checkErr doOp va vb
   in
      updateRC ev val values''

unOp ::  (Double -> Double) 
         -> XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
unOp op ev values v =
   let
      (v', values') = toNumber $ (scalar ev) ev values v
      
      doOp e@(XlError _)  = e
      doOp (XlNumber n)   = XlNumber $ op n
      doOp _              = XlError "#VALUE!"
      
      val = doOp v'
   in
      updateRC ev val values'
\end{code}

\end{document}
