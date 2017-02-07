\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%include Xl_format.lhs

\begin{document}

\title{An interpreter modelling spreadsheet semantics}
\author{Hisham Muhammad}

\maketitle{}

This is an interpreter designed to model the core semantics of spreadsheets,
with a focus on the dataflow language at its core. We chose to model most
closely the semantics of LibreOffice, which is the spreadsheet for which the
most detailed specification documents are available.

Our intention here is to illustrate the various design decisions that go into
specifying precise semantics for a spreadsheet containing a realistic set of
features, showcasing how complexity arises from what is usually seen as a
conceptually simple language. We believe that this helps to explain the number
of incompatibilities between different implementations that we found and
described in our work.

This implementation uses only standard modules included in the Haskell Platform:

\begin{code}

module XlInterpreter where

import Data.Char (ord, chr)
import Data.Fixed
import Data.List (foldl')
import Data.Map.Strict as Map (Map, foldlWithKey, member, insert, empty, lookup)
import Data.Set as Set (Set, insert, member, singleton)
import Debug.Trace

\end{code}

\section{Representation of programs}

A spreadsheet program (called a ``worksheet'') is represented with the
|XlWorksheet| data type in our model, which contains a map from row-column
coordinates to cells.

In modern spreadsheet applications, a complete document is a set of worksheets
(called a workbook). For simplicity, we did not implement support for multiple
worksheets since this does not affect evaluation significantly.

\begin{code}

data XlWorksheet = XlWorksheet XlCells
   deriving Show

type XlCells = Map.Map XlRC XlCell

\end{code}

We represent row-column pairs with the notation |XlRC|. It contains a pair
of addresses, representing row and column, and each of which may be absolute
(represented as |XlAbs n|) or relative (represented as |XlRel n|).

\begin{code}

data XlRC = XlRC XlAddr XlAddr
   deriving (Eq, Ord)

data XlAddr  =  XlAbs  Int  -- (absolute address)
             |  XlRel  Int  -- (relative address)
   deriving (Eq, Ord)

\end{code}

Cells contain formulas. It is a property of the cell the indication if the formula
will be evaluated normally or as an "array formula". In the spreadsheet interface,
a single array formula is presented as covering a range of cells. In our interpreter,
we replicate the formula in each cell of the range, and annotate it with an
$(x, y)$ coordinate pair indicating which element of the range matrix they represent,
with the top-left cell being entry $(0, 0)$.

\begin{code}

data XlCell  =  XlCell    XlFormula
             |  XlAFCell  XlFormula (Int, Int)
   deriving Show

\end{code}

A formula, on its turn, may be a literal value, a reference to another cell, a
reference to a range of cells, or a function, which has a name and a list of
arguments. Our interpreter, thus, manipulates expressions as trees of |XlFun|
nodes, assuming that the textual formula language has already been parsed into
this format.

\begin{code}

data XlFormula  =  XlLit  XlValue
                |  XlRef  XlRC
                |  XlRng  XlRC XlRC
                |  XlFun  String [XlFormula]
   deriving Show

\end{code}

Finally, values are numbers, strings, booleans, errors and matrices of
literals. We represent all matrices as 2-dimensional, stored as a list of
lists, which each inner list representing a row (a unidimensional array is a
2-dimensional matrix with a single row).

\begin{code}

data XlValue  =  XlNumber   Double
              |  XlString   String
              |  XlBoolean  Bool
              |  XlError    String
              |  XlMatrix   [[XlValue]]
   deriving Eq

\end{code}

For convenience we define a few instances of the |Show| type class that will
prove useful later when running the interpreter. In particular, for display
purposes we convert absolute row-column coordinates to the familiar ``A1''
notation.\footnote{We made a simplification here by presenting absolute
coordinates using strings such as @B5@. Such an identifier would actually
represent a relative coordinate, with @$B$5@ being the absolute equivalent.
The @A1@ notation hides the fact that coordinates in spreadsheets are relative
by default (which explains their behavior when copying and pasting cells).}

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

The state of a spreadsheet is the pairing of the map of cells, which stores
the cells and their contents (that is, the formulas), and the map of values,
which stores the computed value for each cell.

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

The execution of a spreadsheet is demand-driven. The user triggers the
evaluation by editing a cell, which causes its value to be recomputed. When
computing the value of a cell, other cells may be referenced, so they are
computed as well, and the process continues recursively. Conversely,
other cells may reference the newly-edited cell, so their values need
to be recomputed as well.

\subsection{Main loop}

Since we are interested in the dynamic semantics (that is, what happens with
the program state over time as it runs), we model our interpreter as a loop of
evaluation steps. The function |runEvents| implements this loop, taking as
inputs a worksheet (a spreadsheet document containing the initial contents of
cell formulas) and a list of events. For each event, it calls the main
evaluation function, |runEvent|, until it produces the final state, containing
the resulting cells and their values.

\begin{code}
runEvents :: XlWorksheet -> [XlEvent] -> XlState
runEvents sheet@(XlWorksheet cells) events =
   foldl' runEvent (XlState cells Map.empty) events
\end{code}

When we process an event in |runEvent|, we need to update the cells entered
and then perform the necessary recalculations. Since we are not concerned with
performance and formulas are in principle purely functional (which is not true
in real-world spreadsheets due to functions such as @TODAY@ which accesses the
system clock, but is true in our interpreter), we simply discard the previous
map of values and recompute all cells in the worksheet. One way to avoid this
computational expense would be to maintain data structures that keep track of
reverse dependencies for each cell, but we avoid this optimization here for
simplicity. Real-world spreadsheets further restrict the recalculation by
limiting it to cells which are currently visible in their user
interface.\footnote{We were able to empirically verify this when we produced a
spreadsheet with a formula that crashed LibreOffice. The application only
crashed when the offending cell was scrolled into view.}

Still, in the auxiliary accumulator function |acc| we avoid recalculating a
cell if it was already calculated in this pass as a dependency of a previous
cell. Also, we keep track of which cells are currently being visited for
detecting circular references.

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

To iterate over ranges, we use a custom folding function, which loops over
the 2-dimensional range applying two accumulator functions: one which runs on
each cell, and one that runs as each row is completed.

\begin{code}
foldRange ::  XlRC -> XlRC -> XlRC     -- base coordinate and addresses for the range
              -> r                     -- a zero-value for the fold as a whole
              -> (r -> c)              -- an initializer function for each row
              -> (c -> XlRC -> c)      -- accumulator function to run on each cell
              -> (r -> Int -> c -> r)  -- accumulator function to run on each complete row
              -> r
foldRange rc from to zero zeroRow cellOp rowOp =
   let
      (minR, minC, maxR, maxC) = minMaxRCs rc from to
      handleRow accRow r = rowOp accRow r valRow
         where
            handleCell accCell c = cellOp accCell (XlRC (XlAbs r) (XlAbs c))
            valRow = foldl' handleCell (zeroRow accRow) [minC..maxC]
   in
      foldl' handleRow zero [minR..maxR]
\end{code}

It is important to note that for array formulas, |updateCells| expands a
single array formula spanning a range of cells into a number of individual
|XlAFCell| entries in the |cells| map, each of them containing the $(x, y)$
coordinates to indicate their relative position in the rectangular range to
which the array formula was applied. 

This makes two important assumptions. First, that we will able later to
compute each position of an array formula individually. This assumption is not
critical. At worst, it would wasteful in cases such as matrix multiplication,
where each cell would cause the whole matrix to be calculated and then
converted down to the scalar corresponding to the cell's position. 

The second assumption is that the computation of a given position $(x, y)$ in
an array formula's range is independent of the total size of the range as
specified when the user selects a range when entering the array formula. In
general, this assumption holds in spreadsheet applications, but we were able
to identify corner cases in Excel where an array formula returns different
results when entered in a single cell versus being entered in a range. For
example, assuming @A1@ contains the string @C1@, @B1@ contains the string
@D1@, @C1@ contains @9@ and @D1@ contains @16@, entering
@=SQRT(INDIRECT(A1:B1))@ in cell @E2@ results in the value @3@; but entering
the same formula with the range @E2:F2@ selected causes the value of both
cells to be @#VALUE!@. In LibreOffice (and in our interpreter), they evaluate
to @3@ and @4@. By behaving differently according to the range size selected
during initial entry, Excel adds a dependency to the calculation of cells that
is invisible in its UI. We avoid this by using calculation strategies
similar to those in LibreOffice and Google Sheets.

\section{Utility functions}

A few utility functions for manipulating coordinates are used throughout the interpreter. We
will quickly introduce them here.

Function |minMaxRCs| obtains the top-left and bottom-right absolute coordinates
of a range, given two opposite edge addresses and a base absolute address.

\begin{code}
minMaxRCs :: XlRC -> XlRC -> XlRC -> (Int, Int, Int, Int)
minMaxRCs myRC rcFrom rcTo =
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

Function |toAbs| converts a relative address to an absolute one, using another
absolute address as a reference.

\begin{code}

toAbs :: XlRC -> XlRC -> XlRC
toAbs ref@(XlRC rb cb) cell@(XlRC r c) = XlRC (absAddr rb r) (absAddr cb c)
   where 
      absAddr :: XlAddr -> XlAddr -> XlAddr
      absAddr  _               c@(XlAbs _)  = c
      absAddr  (XlAbs b)       (XlRel c)    = XlAbs (b + c)
      absAddr  b@(XlRel _)     _            = error ("toAbs: ref must be absolute, got " ++ show b)

\end{code}

Function |toRC| converts Excel-style addresses in ``A1'' alphanumeric format
to the internal row-column numeric format. (For simplicity, we only support
rows A to Z.)

\begin{code}
toRC :: String -> XlRC
toRC (l:num) = XlRC (XlAbs ((read num) - 1)) (XlAbs ((ord l) - 65)) 
\end{code}

\section{Calculating cells}

To calculate the value of a cell we evaluate the cell's formula, potentially
recursing to evaluate other cells referenced by that formula. The |calcCell|
takes as arguments a set of cell addresses currently being recursively visited
(to detect cycles), the table of cell formulas, the current table of values,
the base address of the cell and the cell to compute. The function produces
the calculated value of the cell along with the map of all values, since other
calls may have been computed along the way). 

\begin{code}

calcCell :: Set XlRC -> XlCells -> XlValues -> XlRC -> XlCell -> (XlValue, XlValues)

\end{code}

A major complication in the semantics of a spreadsheet application is the
fact that there are two distinct modes of evaluation: one for regular formulas,
and one for array formulas. Further, different kinds of functions in formulas
evaluate their arguments in different ways: borrowing from the terminology
of the language Perl, some functions evaluate their arguments in a scalar
context (that is, they expect their arguments to produce a scalar value),
and some evaluate arguments in an array context. This gives us four evaluation
rules in total.

This is the core of the incompatibility between spreadsheet formula languages.
As our examples [earlier in the thesis] demonstrate, each application applies
a different set of rules as to when to switch to array evaluation, and to what
to do in each evaluation mode. In our implementation, we modularized these
decisions into a number of functions implementing different ways of evaluating
a formula, in array and scalar contexts.

Then, to represent an evaluation mode, we define a data type |XlEvaluator|
which, besides carrying a few context values for convenience, includes a
coercion function |toScalar| to obtain a scalar function according to the
context of a cell (as we will see in more detail below), and two evaluation
functions, one for each of the possible evaluation contexts: |array| and
|scalar|.

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

Note that the presence of different evaluation rules affects not only array
formulas. As illustrated in [Figure], Excel performs array-style evaluation in
sub-formulas for certain functions even when not in array formula mode.

We opted to implement evaluation functions that follow the OpenDocument
specification. With this, we achieved a good (but not full) degree of
compatibility with LibreOffice in the subset of spreadsheet features
implemented in this interpreter.

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

When a function expects a scalar argument and is given a non-scalar such as a
literal matrix or a range, coercion is attempted.

For array literals, element $(0, 0)$ is returned. Empty arrays have
inconsistent behavior across spreadsheets. Here, we simply return the @#REF!@
error, replicating the behavior of Google Sheets. When given an empty arrays,
Excel rejects the formula, pops a message box alerting the user and does not
accept the entry. Excel Online does not display a message, but marks the cell
with a red dashed border. LibreOffice exhibits a very inconsistent behavior:
an empty cell by itself displays as an empty cell; @=10/{}@ evaluates to
@#VALUE!@ but both @=ABS({})@ and @=ABS(10/{})@ evaluate to @0@, but
@=ABS(A1)@ where A1 is @{}@) evaluates to @#VALUE!@.

For ranges,the resulting value depends on the shape of the range and position
where the formula was entered. If the range is a vertical ($n \times 1$) or
horizontal ($1 \times n$) array, the evaluation follows an "intersection"
rule: the value returned is that of the element of the range that is aligned
in the perpendicular direction with the position of the formula. For example,
for a formula in cell @G5@ that references @A1@ in a scalar context, the value
in @A5@ will be returned. Likewise, if that same cell references @E1:K1@, the
value obtained will be that in cell @G1@. If there is no intersection or if
the range has any other shape, @#VALUE!@ is returned.

\begin{code} 

intersectScalar :: XlRC -> Int -> Int -> XlFormula -> XlFormula
intersectScalar myRC@(XlRC (XlAbs r) (XlAbs c)) _ _ formula =
   case formula of
      XlLit (XlMatrix [])   -> XlLit (XlError "#REF!")
      XlLit (XlMatrix [[]]) -> XlLit (XlError "#REF!")
      XlLit (XlMatrix mtx)  -> XlLit (head (head mtx))
      XlRng rcFrom rcTo ->
         let
            (fromR, fromC, toR, toC) = minMaxRCs myRC rcFrom rcTo
         in
            if fromC == toC
            then
               if (r >= fromR) && (r <= toR)
               then XlRef (XlRC (XlAbs r) (XlAbs fromC))
               else XlLit (XlError "#VALUE!")
            else if fromR == toR
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
                  (fromR, fromC, toR, toC) = minMaxRCs (rc ev) rcFrom rcTo
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
                  (fromR, fromC, toR, toC) = minMaxRCs (rc ev) rcFrom rcTo
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
               (fromR, fromC, toR, toC) = minMaxRCs myRC rcFrom rcTo
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
         -- 2.1.4) If none of the other rules apply @#N/A@
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
