\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%BEGIN LYX PREAMBLE
%include polycode.fmt
%include Xl_format.lhs
%END LYX PREAMBLE

\begin{document}

\title{An interpreter modelling spreadsheet semantics}
\author{Hisham Muhammad}

\maketitle{}

This is an interpreter designed to model the core semantics of spreadsheets,
with a focus on the dataflow language at its core. Our intention here is to
illustrate the various design decisions that go into specifying precise
semantics for a spreadsheet containing a realistic set of features, showcasing
how complexity arises from what is usually seen as a conceptually simple
language. We believe that this helps to explain the number of
incompatibilities between different implementations that we found and
described in our work.

We chose to model most closely the semantics of LibreOffice, which is the
spreadsheet for which the most detailed specification documents are available.

This section was written in Literate Haskell, and is a description of the
entire interpreter, including its complete listings. The text was written so
that it should be understandable without a detailed reading of the source
code, but the sources are nevertheless included for completeness and as
supporting material for the interested reader. The source code in @.lhs@
format (\LaTeX~with embedded Haskell) is also available at \url{https://hisham.hm/thesis/}.

%BEGIN LYX TEXT

This implementation uses only standard modules included in the Haskell Platform:

\begin{code}

module XlInterpreter where

import Data.Char (ord, chr, toUpper)
import Data.Fixed
import Data.List (foldl')
import Data.Map.Strict as Map (Map, foldlWithKey, member, empty, lookup)
import Data.Set as Set (Set, member, singleton)
import Data.Map.Strict (insert)
import qualified Data.Set as Set (insert)

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

Cells contain formulas. As explained in Section \ref{sub:Array-formulas},
formulas can be evaluated in a special mode called ``array formula''. The
indication if the formula will be evaluated as an array formula is a property
of the cell, not the formula.

In the spreadsheet interface, a single array formula is presented as covering
a range of cells. In our interpreter, we replicate the formula in each cell of
the range, and annotate it with an $(x, y)$ coordinate indicating which
element of the range matrix they represent, with the top-left cell being entry
$(0, 0)$. We will call this pair the offset of a cell in an array formula.

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
2-dimensional matrix with a single row). We also have a special value
for an empty cell, due to its special coercion rules (implemented in
Section~\ref{typeconv}).

\begin{code}

data XlValue  =  XlNumber   Double
              |  XlString   String
              |  XlBool     Bool
              |  XlError    String
              |  XlMatrix   [[XlValue]]
              |  XlEmpty
   deriving Eq

\end{code}

For convenience we define a few instances of the |Show| type class that will
prove useful later when running the interpreter. In particular, for display
purposes we convert absolute row-column coordinates to the familiar ``A1''
notation.\footnote{We made a simplification here by presenting absolute
coordinates using strings such as @B5@. In spreadsheets, such an identifier
actually represents a relative coordinate, with @$B$5@ being the absolute
equivalent. The @A1@ notation hides the fact that coordinates in spreadsheets
are relative by default (which explains their behavior when copying and
pasting cells).}

\begin{code}
instance Show XlRC where
   show (XlRC r@(XlAbs rn) c@(XlAbs cn)) =
      "<" ++ [chr (cn + 65)] ++ (show (rn + 1)) ++ ">"
   show (XlRC r c) =
      "R" ++ show r ++ "C" ++ show c

instance Show XlValue where
   show (XlNumber d)  = num2str d
   show (XlString s)  = show s
   show (XlBool b)    = show b
   show (XlError e)   = show e
   show (XlMatrix m)  = show m
   show XlEmpty       = ""

instance Show XlAddr where
   show (XlAbs n) = show n
   show (XlRel n) = "[" ++ show n ++ "]"
\end{code}

\section{Representation of states}

The state of a spreadsheet consists of the map of cells, which stores the cells
and their contents (that is, the formulas), and the map of values, which
stores the computed value for each cell. Both are indexed by row-column coordinates.

\begin{code}

data XlState = XlState XlCells XlValues

type XlValues = Map.Map XlRC XlValue

\end{code}

From a programming language perspective, interaction with a spreadsheet
consists exclusively of replacing formulas in cells. We represent these as
events that contain the absolute coordinates and the formula to be entered to
a cell. In the case of array formulas, a rectangular range (denoted by the
top-left and bottom-right cells) covering one or more cells must be given. A
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

Unlike the interpreter modelling Pure Data in Chapter \ref{chap:Pure-Data}, we
return only the final state, since inspecting the final result of the
spreadsheet is usually sufficient for understanding its behavior. Tracing the
intermediate results is an easy modification if desired.

\begin{code}
runEvents :: XlWorksheet -> [XlEvent] -> XlState
runEvents sheet@(XlWorksheet cells) events =
   foldl' runEvent (XlState cells Map.empty) events
\end{code}

When we process an event in |runEvent|, we need to update the cells that were
entered and then perform the necessary recalculations. Since we are not
concerned with performance and formulas are in principle purely functional
(which is not true in real-world spreadsheets due to functions such as @TODAY@
which reads the system clock, but is true in our interpreter), we simply
discard the previous map of values and recompute all cells in the worksheet.
One way to avoid this computational expense would be to maintain data
structures that keep track of reverse dependencies for each cell, but we avoid
this optimization here for simplicity. Real-world spreadsheets further
restrict the recalculation by limiting it to cells which are currently visible
in their user interface.\footnote{We were able to empirically verify this when
we produced a spreadsheet with a formula that crashed LibreOffice. The
application only crashed when the offending cell was scrolled into view.}

Our interpreter does avoid recalculating a cell if it was already calculated
in the current pass as a dependency of a previous cell. Also, it keeps track
of which cells are currently being visited, for detecting circular references.

\begin{code}
runEvent :: XlState -> XlEvent -> XlState
runEvent env@(XlState cells _) event =
   let 
      cells' = updateCells cells event
      
      acc :: XlValues -> XlRC -> XlCell -> XlValues
      acc vs rc cell =
         if Map.member rc vs
         then vs
         else
            let (v', vs') = calcCell (Set.singleton rc) cells' vs rc cell
            in insert rc v' vs' 
   in
      XlState cells' (Map.foldlWithKey acc Map.empty cells')
\end{code}

An event may update a single cell in case of a regular formula, or many cells
at a time in case of an array formula applied over a range. Function
|updateCells| covers both cases:

\begin{code}
updateCells cells event@(XlSetFormula rc fml) =
   insert rc (XlCell fml) cells

updateCells cells event@(XlSetArrayFormula rcFrom rcTo fml) =
   fst $ foldRange rcFrom rcFrom rcTo (cells, (0, 0)) id cellOp rowOp
      where
         cellOp (cells, (x, y)) rc  = (insert rc (XlAFCell fml (x, y)) cells, (x + 1, y))
         rowOp _ r (cells, (x, y))  = (cells, (0, y + 1))
\end{code}

To iterate over ranges, we define a folding function |foldRange|, which
loops over the 2-dimensional range applying two accumulator functions: one
which runs on each cell, and one that runs as each row is completed.

\begin{code}
foldRange ::  XlRC -> XlRC -> XlRC     -- cell position and addresses for the range
              -> r                     -- a zero-value for the fold as a whole
              -> (r -> c)              -- an initializer function for each row
              -> (c -> XlRC -> c)      -- function to run on each cell
              -> (r -> Int -> c -> r)  -- function to run on each complete row
              -> r
foldRange pos rcFrom rcTo zero zeroRow cellOp rowOp =
   let
      (minR, minC, maxR, maxC) = toAbsRange pos rcFrom rcTo
      handleRow accRow r = rowOp accRow r vRow
         where
            handleCell accCell c = cellOp accCell (XlRC (XlAbs r) (XlAbs c))
            vRow = foldl' handleCell (zeroRow accRow) [minC..maxC]
   in
      foldl' handleRow zero [minR..maxR]
\end{code}

It is important to note that, when handling array formulas, |updateCells|
expands a single array formula spanning a range of cells into a number of
individual |XlAFCell| entries in the |cells| map, each of them containing the
$(x, y)$ offset to indicate their relative position in the rectangular
range to which the array formula was applied. 

This makes two important assumptions. First, that it is possible to compute
each position of an array formula individually. This assumption is not
critical. At worst, it would wasteful in cases such as matrix multiplication,
where each cell would cause the whole matrix to be calculated and then
converted down to the scalar corresponding to the cell's position.

The second assumption is that the computation of a given cell from an array
formula's range is independent of the total size of the range as specified by
the user when the array formula was created. In general, this assumption holds
in spreadsheet applications, but we were able to identify corner cases in
Excel where an array formula returns different results when entered in a
single cell versus being entered in a range. For example, assuming @A1@
contains the string @C1@, @B1@ contains the string @D1@, @C1@ contains @9@ and
@D1@ contains @16@, entering @=SQRT(INDIRECT(A1:B1))@ in cell @E2@ results in
the value @3@; but entering the same formula with the range @E2:F2@ selected
causes the value of both cells to be @#VALUE!@. In LibreOffice (and in our
interpreter), they evaluate to @3@ and @4@. By behaving differently according
to the range size selected during initial entry, Excel adds a dependency to
the calculation of cells that is invisible in its UI. This interpreter avoids
this problem by using calculation strategies similar to those in LibreOffice
and Google Sheets.

\subsection{Resolving addresses}

Relative coordinates are used extensively in spreadsheets, but whenever they
are used they need to be resolved into absolute addresses. Also, whenever the
interpreter uses ranges, it needs to ensure that they are normalized as
absolute coordinates with the top-left cell first and the bottom-right cell
second. 

When relative addresses are given, they are resolved relative to the
coordinate of the cell being evaluated, which we will refer throughout as the
cell's position.

Functions |toAbs| and |toAbsRange| normalize coordinates and ranges, respectively:

\begin{code}
toAbs :: XlRC -> XlRC -> XlRC
toAbs pos@(XlRC rp cp) cell@(XlRC r c) = XlRC (absAddr rp r) (absAddr cp c)
   where 
      absAddr :: XlAddr -> XlAddr -> XlAddr
      absAddr  _               c@(XlAbs _)  = c
      absAddr  (XlAbs b)       (XlRel c)    = XlAbs (b + c)
      absAddr  b@(XlRel _)     _            = undefined

toAbsRange :: XlRC -> XlRC -> XlRC -> (Int, Int, Int, Int)
toAbsRange pos rcFrom rcTo =
   let
      XlRC (XlAbs fromR) (XlAbs fromC)  = toAbs pos rcFrom
      XlRC (XlAbs toR)   (XlAbs toC)    = toAbs pos rcTo
      minR  = min  fromR  toR
      maxR  = max  fromR  toR
      minC  = min  fromC  toC
      maxC  = max  fromC  toC
   in
      (minR, minC, maxR, maxC)
\end{code}

\section{Calculating cell values}
\label{calccell}

To determine the value of a cell, the interpreter evaluates the cell's
formula, potentially recursing to evaluate other cells referenced by that
formula. The |calcCell| function takes as arguments a set of cell addresses
currently being recursively visited (to detect cycles), the table of cell
formulas, the current table of values, the cell position and the actual cell
to compute. The function produces the calculated value of the cell along with
the map of all values, since other calls may have been computed along the
way. 

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
As our examples in Section~\ref{sub:Array-formulas} demonstrate, each
application uses a different set of rules as to when to switch to array
evaluation, and to what to do in each evaluation mode.

Note that the presence of different evaluation rules affects not only array
formulas. As illustrated in Figure~\ref{fig:Formula-evaluation-incompatibilities},
Excel performs array-style evaluation in sub-formulas for certain functions
even when not in array formula mode.

In our implementation, we modularized these decisions into a number of
functions implementing different ways of evaluating a formula, in array and
scalar contexts.

Then, to represent an evaluation mode, the interpreter features a data type
|XlEvaluator| which, besides carrying a few context values for convenience,
includes a coercion function |eToScalar| to obtain a scalar function according
to the context of a cell (as we will see in more detail below), and two
evaluation functions, one for each of the possible evaluation contexts:
|eArray| and |eScalar|.

\begin{code}

data XlEvaluator = XlEvaluator {
   ePos       :: XlRC,
   eOffset    :: (Int, Int),
   eVisiting  :: Set XlRC,
   eCells     :: XlCells,
   eToScalar  :: XlRC -> (Int, Int) -> XlFormula -> XlFormula,
   eArray     :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues),
   eScalar    :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
}

\end{code}

We opted to implement evaluation functions that follow the OpenDocument
specification. With this, we achieved a good (but deliberately not full)
degree of compatibility with LibreOffice in the subset of spreadsheet features
implemented in this interpreter.

For calculating the value of a regular cell, the interpreter employs an evaluator
that uses functions |intersectScalar| to convert non-scalar to scalars,
|evalScalarFormula| for evaluating scalar arguments, and |evalFormula| for
evaluating non-scalar arguments. We will see the definition of these functions
in Section~\ref{regularcells}. Once the evaluator is defined, |calcCell|
triggers the scalar evaluation function on the formula.

\begin{code}

calcCell visiting cells vs pos@(XlRC (XlAbs r) (XlAbs c)) (XlCell formula) =
   evalScalarFormula ev vs formula
   where
      ev = XlEvaluator {
         ePos       = pos,
         eOffset    = (0, 0),
         eCells     = cells,
         eVisiting  = visiting,
         eToScalar  = intersectScalar,
         eScalar    = evalScalarFormula,
         eArray     = evalFormula
      }

\end{code}

For calculating cells marked as array formulas, the interpreter uses a
different evaluator. For coercing non-scalars into scalars, it uses a
different function, |matrixToScalar|. For scalar evaluation of arguments, it
uses the same function |evalScalarFunction| as above, but for non-scalar
evaluation, it uses |iterateFormula|. Both |matrixToScalar| and
|iterateFormula| will be defined in Section~\ref{arrayformulas}.

The implementation of |calcCell| for array formulas also triggers the
calculation by applying this mode's scalar evaluator, but here the result
value is further filtered through a coercion function (|scalarize|), to ensure
that a scalar value is ultimately displayed in the cell.

\begin{code}

calcCell visiting cells vs pos (XlAFCell formula (x, y)) =
   scalarize ev $ (eScalar ev) ev vs formula
   where
      ev = XlEvaluator {
         ePos       = pos,
         eOffset    = (x, y),
         eCells     = cells,
         eVisiting  = visiting,
         eToScalar  = matrixToScalar,
         eScalar    = evalScalarFormula,
         eArray     = iterateFormula
      }

scalarize :: XlEvaluator -> (XlValue, XlValues) -> (XlValue, XlValues)
scalarize ev (v, vs) = (v', vs)
   where
      (XlLit v') = matrixToScalar (ePos ev) (eOffset ev) (XlLit v)

\end{code}

\subsection{Regular cell evaluation}
\label{regularcells}

When the interpreter evaluates a formula in a scalar context, it runs the
evaluator's scalar conversion function on the formula prior to evaluating it
proper. If the formula is an array or a range, it will be converted to a
scalar. If it is a scalar or a function, it will be evaluated as-is.

\begin{code}

evalScalarFormula ev vs formula =
   evalFormula ev vs formula'
   where
      formula' = (eToScalar ev) (ePos ev) (eOffset ev) formula

\end{code}

The conversion function for regular cells, |intersectScalar|, is defined as
follows.

For array literals, element $(0, 0)$ is returned. Empty arrays have
inconsistent behavior across spreadsheets. When given an empty array, Excel
rejects the formula, pops a message box alerting the user and does not accept
the entry. Excel Online does not display a message, but marks the cell with a
red dashed border. LibreOffice exhibits a very inconsistent behavior: @={}@
displays as an empty cell; @=10/{}@ evaluates to @#VALUE!@ but both @=ABS({})@
and @=ABS(10/{})@ evaluate to @0@; however, @=ABS(A1)@ where @A1@ is @{}@
evaluates to @#VALUE!@. In our interpreter, we simply return the @#REF!@
error for all uses of @{}@, replicating the behavior of Google Sheets. 

For ranges, the resulting value depends on the shape of the range and the
position in the spreadsheet grid where the formula was entered. If the range
is a vertical ($n \times 1$) or horizontal ($1 \times n$) array, the
evaluation follows an ``intersection'' rule: the value returned is that of the
element of the range that is perpendicularly aligned with the position of the
formula. For example, for a formula in cell @G5@ that references @A1@ in a
scalar context, the value in @A5@ will be returned. Likewise, if that same
cell @G5@ references @E1:K1@, the value obtained will be that in cell @G1@. If
there is no intersection or if the range has any other shape, @#VALUE!@ is
returned.

\begin{code} 

intersectScalar :: XlRC -> (Int, Int) -> XlFormula -> XlFormula
intersectScalar pos@(XlRC (XlAbs r) (XlAbs c)) _ formula =
   case formula of
   XlLit (XlMatrix [])    -> XlLit (XlError "#REF!")
   XlLit (XlMatrix [[]])  -> XlLit (XlError "#REF!")
   XlLit (XlMatrix m)     -> XlLit (head (head m))
   XlRng rcFrom rcTo      ->
      case toAbsRange pos rcFrom rcTo of
      (fromR, fromC, toR, toC) 
         | (fromC == toC) && (r >= fromR) && (r <= toR)  -> XlRef (XlRC (XlAbs r) (XlAbs fromC))
         | (fromR == toR) && (c >= fromC) && (c <= toC)  -> XlRef (XlRC (XlAbs fromR) (XlAbs c))
      _ -> XlLit (XlError "#VALUE!")
   f -> f

\end{code}

\subsection{Cell evaluation for array formulas}
\label{arrayformulas}

When a cell is marked as an array formula, it follows a different evaluation
process. As we saw in the definition of the array formula evaluator in
function |calcCell| (Section~\ref{calccell}), for scalar contexts we use
the same evaluation function as in regular cells, |evalScalarFormula|.
However, in array formulas this function uses a different conversion function:
|eToScalar| is defined as |matrixToScalar|.

Function |matrixToScalar| extracts a scalar value from a non-scalar based on
the offset $(x, y)$ relative to the range for which the array formula was
defined. This way, as |runEvent| calculates cell values for each position of
an array formula, the evaluation of each cell will extract a different value
from non-scalars produced during the calculation of the formula. For example,
if we enter @=A1:B2@ as an array formula in range @D10:E11@, cell @D11@ has
offset $(1, 0)$ and will obtain the value of cell @B1@.

The area designated by the user for an array formula does not necessarily have
the same dimensions as the non-scalar being displayed in it. The OpenDocument
specification lists a series of rules for filling the exceeding cells, which
the |displayRule| function below implements. Excel and LibreOffice also
implement these rules; Google Sheets does not.

\begin{code} 

matrixToScalar :: XlRC -> (Int, Int) -> XlFormula -> XlFormula
matrixToScalar pos (x, y) f =
   case f of
      XlLit (XlMatrix m) ->
         displayRule x y (foldl' max 0 (map length m)) (length m)
                     (\x y -> XlLit $ m !! y !! x)
      XlRng rcFrom rcTo ->
         displayRule x y (1 + toC - fromC) (1 + toR - fromR)
                     (\x y -> XlRef (XlRC (XlAbs (fromR + y)) (XlAbs (fromC + x))))
            where
               (fromR, fromC, toR, toC) = toAbsRange pos rcFrom rcTo
      f -> f
   where
      displayRule ::  Int -> Int -> Int -> Int -> (Int -> Int -> XlFormula)
                      -> XlFormula
      displayRule x y sizeX sizeY getXY 
         | sizeX > x   &&  sizeY > y   = getXY x y
         | sizeX == 1  &&  sizeY == 1  = getXY 0 0
         | sizeX == 1  &&  x > 0       = getXY 0 y
         | sizeY == 1  &&  y > 0       = getXY x 0
         | otherwise                   = XlLit $ XlError "#N/A"
\end{code}

Function |iterateFormula| implements the special evaluation mode for array
formulas. When given a function where any argument is a range or a matrix, it
produces a matrix with results. It does this by first checking each argument
and determining the maximum dimensions used by an argument (|maxX| and
|maxY|). Then, it iterates from $(0, 0)$ to (|maxX|, |maxY|), evaluating the
function in scalar context once for each entry. In each evaluation, it uses a
modified version of the list of arguments, in which each non-scalar argument
is converted to a scalar based on the current $(x, y)$ offset.

If the given function has no non-scalar arguments, it is evaluated normally by
|evalFormula|.

\begin{code}
iterateFormula :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
iterateFormula ev vs (XlFun name args) =
   if maxX > 1 || maxY > 1
   then (\(m, vs') -> (XlMatrix m, vs')) $ foldl' doRow ([], vs) [0..maxY-1]
   else evalFormula ev vs (XlFun name args)
   where
      maxY = foldl' getY 1 args
         where
            getY a (XlLit (XlMatrix m))  = max a (length m)
            getY a (XlRng rcFrom rcTo)   = max a (1 + toR - fromR)
               where
                  (fromR, _, toR, _) = toAbsRange (ePos ev) rcFrom rcTo
            getY a _ = a
      maxX = foldl' getX 1 args
         where
            getX a (XlLit (XlMatrix m)) = max a (maxRowLength m)
               where
                  maxRowLength :: [[XlValue]] -> Int
                  maxRowLength m = foldl' (\a' row -> max a' (length row)) 1 m
            getX a (XlRng rcFrom rcTo) = max a (1 + toC - fromC)
               where
                  (_, fromC, _, toC) = toAbsRange (ePos ev) rcFrom rcTo
            getX a _ = a
      doRow :: ([[XlValue]], XlValues) -> Int -> ([[XlValue]], XlValues)
      doRow (m, vs) y = appendTo m $ foldl' doCell ([], vs) [0..maxX-1]
         where
            doCell :: ([XlValue], XlValues) -> Int -> ([XlValue], XlValues)
            doCell (row, vs) x = appendTo row $ evalFormula ev vs f'
               where
                  f' = XlFun name (map ((eToScalar ev) (ePos ev) (x, y)) args)
            appendTo xs (v, vs) = (xs ++ [v], vs)

iterateFormula ev vs f = evalFormula ev vs f

\end{code}

\section{Operations}

The last part of the interpreter is function |evalFormula|, which implements
the evaluation of the various operations available in the textual formula
language. Given an evaluator, the current map of values, and a formula, it
produces the calculated value of the formula and a new map of values (since
other cells may be calculated as part of the evaluation of this formula).

\begin{code}
evalFormula :: XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
\end{code}

The function |evalFormula| implements the various language constructs as
follows.

\subsection{Literals, references and ranges}

When a formula is just a literal, its value is returned and the
map of cell values remains unchanged.

\begin{code}
evalFormula ev vs (XlLit v) = (v, vs)
\end{code}

When a formula is a reference to another cell, |evalFormula| first converts
the reference address to its absolute value relative to the cell's position.
Then, it detects circular references by checking the |eVisiting| set of the
evaluator. If the reference is valid, it checks in the map of values if the
value was already calculated. If the cell is unset, we return the special
value |XlEmpty|. Finally, if the cell contains a formula which needs to be
calculated, we calculate it with |calcCell| and store the resulting value in
an updated map of values.

\begin{code}
evalFormula ev vs (XlRef ref') =
   let
      ref = toAbs (ePos ev) ref'
   in
      if ref `Set.member` (eVisiting ev)
      then (XlError "#LOOP!", vs)
      else 
         case Map.lookup ref vs of
         Just v   -> (v, vs)
         Nothing  ->
            case Map.lookup ref (eCells ev) of
            Nothing    -> (XlEmpty, vs)
            Just cell  -> 
               (v', vs'')
               where
                  (v', vs') = calcCell (Set.insert ref (eVisiting ev)) (eCells ev) vs ref cell
                  vs'' = insert ref v' vs'
\end{code}

For evaluating ranges, |evalFormula| uses |foldRange| to iterate over the
range, invoking the scalar evaluation function (|eScalar|) for each element,
producing a matrix of values.

\begin{code}
evalFormula ev vs (XlRng from to) = 
   let
      (m, vs') = foldRange (ePos ev) from to ([], vs) zeroRow cellOp rowOp
         where
            zeroRow :: ([[XlValue]], XlValues) -> ([XlValue], XlValues)
            zeroRow (_, vs) = ([], vs)
   
            cellOp :: ([XlValue], XlValues) -> XlRC -> ([XlValue], XlValues)
            cellOp (row, vs) rc =
               addToRow $ (eScalar ev) ev vs (XlRef rc)
                  where addToRow (v, vs') = (row ++ [v], vs')
   
            rowOp ::  ([[XlValue]], XlValues) -> Int -> ([XlValue], XlValues)
                      -> ([[XlValue]], XlValues)
            rowOp (m, _) r (row, vs) = (m ++ [row], vs)
   in
      (XlMatrix m, vs')
\end{code}

\subsection{\texttt{IF}, \texttt{AND}, and \texttt{OR}}

The @IF@ function takes three arguments. It tests the first argument, and if evaluates
to |XlBool True| it evaluates the second argument and returns it; otherwise, it
evaluates and returns the third argument. Note that arguments are evaluated lazily,
as is typical in constructs of this type in programming languages.

\begin{code}
evalFormula ev vs (XlFun "IF" [i, t, e]) =
   let
      (vi, vsi) = toBool $ (eScalar ev) ev vs i
      (vr, vsr) = 
         case vi of
         (XlError _)     -> (vi, vsi)
         (XlBool True)   -> (eScalar ev) ev vsi e
         (XlBool False)  -> (eScalar ev) ev vsi t
         _               -> ((XlError "#VALUE!"), vsi)
   in
      (vr, vsr)

\end{code}

The @AND@ and @OR@ functions in spreadsheets, however, are evaluated strictly,
not performing the usual short-circuit expected of them in programming
languages. They always evaluate both arguments, and return and error if either
argument fails.

\begin{code}
evalFormula ev vs (XlFun "AND" [a, b]) =
   let
      (va, vs')  = toBool $ (eScalar ev) ev vs a
      (vb, vs'') = toBool $ (eScalar ev) ev vs' b
      vr = case (va, vb) of
           (XlError _, _)              -> va
           (_, XlError _)              -> vb
           (XlBool True, XlBool True)  -> va
           _                           -> XlBool False
   in
      (vr, vs'')

evalFormula ev vs (XlFun "OR" [a, b]) =
   let
      (va, vs')  = toBool $ (eScalar ev) ev vs a
      (vb, vs'') = toBool $ (eScalar ev) ev vs' b
      vr = case (va, vb) of
           (XlError _, _)    -> va
           (_, XlError _)    -> vb
           (XlBool True, _)  -> va
           (_, XlBool True)  -> vb
           _                 -> XlBool False
   in
      (vr, vs'')
\end{code}

\subsection{\texttt{SUM}}

The @SUM@ function illustrates the use of array evaluation. Each argument is
evaluated using the |eArray| function of the evaluator |ev|, and their results
are added producing the final result |vr|. Thus, when used in an array
formula, the evaluation of its arguments is done using |iterateFormula|
(Section~\ref{arrayformulas}), producing a |XlMatrix| of results that is then
iterated to perform the sum. This allows, for example, to use
@=SUM(SQRT(A1:A10))@ to obtain a sum of squares, even though function @SQRT@
is a scalar function that does not support ranges on its own.

It is worth noting that the coercion rules used by @SUM@ are different from
those used by @+@ (Section~\ref{numops}). While @SUM@ skips string values
(which may appear, for example, as part of a range), the @+@ function attempts
to coerce them into numbers.

\begin{code}
evalFormula ev vs (XlFun "SUM" args) =
   let
      
      doSum  s@(XlString _)  v             = v
      doSum  v             s@(XlString _)  = v
      doSum  (XlBool b)    (XlNumber n)    = XlNumber (bool2num b + n)
      doSum  (XlNumber n)  (XlBool b)      = XlNumber (bool2num b + n)
      doSum  (XlNumber a)  (XlNumber b)    = XlNumber (a + b)
      
      (vr, vsr) = foldl' handle (XlNumber 0, vs) args
         where
            handle (acc, vsacc) arg =
               let
                  (va, vsa) = (eArray ev) ev vsacc arg
                  vsum =
                     case va of
                        XlError _   -> va
                        XlMatrix m  -> foldl' (foldl' (checkErr doSum)) acc m
                        XlBool b    -> checkErr doSum acc va
                        XlNumber n  -> checkErr doSum acc va
                        _           -> XlError "#VALUE!"
               in
                  (vsum, vsa)
   in
      (vr, vsr)
\end{code}

\subsection{\texttt{INDIRECT}}

The @INDIRECT@ function converts a string describing a reference or range
written in ``A1'' notation to the actual reference or range. This feature adds
support for runtime-evaluated indirect connections to the dataflow graph of a
spreadsheet. A cell can effectively act as a pointer to another cell.

% Expand this issue in a discussion on liveness, later in the thesis.

Different spreadsheets vary in their semantics when supporting non-scalar
indirect references. Here, we opted for implementing it in a straightforward
way: we evaluate the argument in a scalar context, coercing it to string, and
then evaluate the indirect reference in a scalar context as well. When used in
an array formula, @INDIRECT@ can handle non-scalar arguments due to the scalar
conversion performed by |matrixToScalar| (Section~\ref{arrayformulas}).

Auxiliary function |toRC| converts addresses in ``A1'' alphanumeric format to
the internal row-column numeric format. For simplicity, this interpreter only
support columns A to Z, and we assume the string is well-formed and do not
perform error checking.

\begin{code}
evalFormula ev vs (XlFun "INDIRECT" [addr]) =
   let
      toRC :: String -> XlRC
      toRC (l:num) = XlRC (XlAbs ((read num) - 1)) (XlAbs ((ord l) - 65)) 

      convert s =
         case break (== ':') s of
         (a1, ':':b2)  -> (XlRng (toRC a1) (toRC b2))
         _             -> (XlRef (toRC s))

      (va, vsa)  = toString $ (eScalar ev) ev vs addr
      (vr, vsr)  = 
         case va of
         XlError e   -> (va, vsa)
         XlString s  -> (eScalar ev) ev vsa (convert s)
         _           -> ((XlError "#VALUE!"), vsa)
   in
      (vr, vsr)
\end{code}

\subsection{String operations}

For illustrative purposes, we define a function that operates on strings: the
substring function @MID@, and the concatenation operator @&@. These are useful
for demonstrating the coercion rules in examples. In particular, it is
interesting to observe how the empty cell coerces to different values: with
@A1@ being empty, @="Hello"&A1@ results in @"Hello"@, and @=1/A1@ results in
@#DIV/0!@.

\begin{code}
evalFormula ev vs (XlFun "MID" [vstr, vstart, vlen]) =
   let
      (vstr',    vs')    = toString  $ (eScalar ev) ev vs    vstr
      (vstart',  vs'')   = toNumber  $ (eScalar ev) ev vs'   vstart
      (vlen',    vs''')  = toNumber  $ (eScalar ev) ev vs''  vlen
      
      doMid (XlString str) (XlNumber start) (XlNumber len) =
         XlString $ take (floor len) $ drop (floor start - 1) str
      doMid _ _ _ = XlError "#VALUE!"
      
      v = doMid vstr' vstart' vlen'
   in
      (v, vs''')
\end{code}

\begin{code}
evalFormula ev vs (XlFun "&" [a, b]) =
   let
      (va, vs')   = toString $ (eScalar ev) ev vs   a
      (vb, vs'')  = toString $ (eScalar ev) ev vs'  b
      
      doConcat  (XlString sa)  (XlString sb)   = XlString (sa ++ sb)
      doConcat  _              _               = XlError "#VALUE!"
      
      v = checkErr doConcat va vb
   in
      (v, vs'')
\end{code}

\subsection{Mathematical operations and equality}
\label{numops}

A few unary and binary mathematical operations are defined here. They all
follow the same pattern, encapsulated as functions |unOp| and |binOp| defined
further below. The division operator additionally checks for division-by-zero,
returning @#DIV/0!@.

\begin{code}

evalFormula ev vs (XlFun "SQRT"  [v])  = unOp   sqrt  ev vs v
evalFormula ev vs (XlFun "ABS"   [v])  = unOp   abs   ev vs v
evalFormula ev vs (XlFun "+" [a, b])   = binOp  (+)   ev vs a b
evalFormula ev vs (XlFun "-" [a, b])   = binOp  (-)   ev vs a b
evalFormula ev vs (XlFun "*" [a, b])   = binOp  (*)   ev vs a b

evalFormula ev vs (XlFun "/" [a, b]) =
   let
      (va,  vs')   = toNumber $ (eScalar ev) ev vs   a
      (vb,  vs'')  = toNumber $ (eScalar ev) ev vs'  b
      
      doDiv  (XlNumber na)  (XlNumber 0)   = XlError "#DIV/0!"
      doDiv  (XlNumber na)  (XlNumber nb)  = XlNumber (na / nb)
      doDiv  _              _              = XlError "#VALUE!"
      
      v = checkErr doDiv va vb
   in
      (v, vs'')

\end{code}

The equality operator is notable in which is does not perform number and
string coercions as the other functions (that is, @=2="2"@ returns @FALSE@).
However, it does coerce booleans to numbers, probably as a compatibility
leftover from when spreadsheets did not have a separate boolean type. The
OpenDocument specification \cite{OASIS2011ODFFormula} states that a conforming
implementation may represent booleans as a subtype of numbers.

\begin{code}
evalFormula ev vs (XlFun "=" [a, b]) =
   let
      (va,  vs')  = (eScalar ev) ev vs  a
      (vb,  vs'') = (eScalar ev) ev vs' b
      
      doEq  (XlNumber na)  (XlNumber nb)  = XlBool (na == nb)
      doEq  (XlString sa)  (XlString sb)  = XlBool (sa == sb)
      doEq  (XlBool ba)    (XlBool bb)    = XlBool (ba == bb)
      doEq  (XlNumber na)  (XlBool bb)    = XlBool (na == bool2num bb)
      doEq  (XlBool ba)    (XlNumber nb)  = XlBool (bool2num ba == nb)
      doEq  _              _              = XlBool False
      
      v = checkErr doEq va vb
   in
      (v, vs'')

evalFormula ev vs (XlFun _ _) = (XlError "#NAME?", vs)

\end{code}

Functions |unOp| and |binOp| are convenience functions that encapsulate the
pattern for common unary and binary numeric functions. They evaluate their
arguments in a scalar context, check if any of the arguments evaluated to an
error, and perform the operation |op|.

\begin{code}
unOp ::  (Double -> Double) 
         -> XlEvaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
unOp op ev vs v =
   let
      (v', vs') = toNumber $ (eScalar ev) ev vs v
      v'' = case v' of
            e@(XlError _)  -> e
            (XlNumber n)   -> XlNumber $ op n
            _              -> XlError "#VALUE!"
   in
      (v'', vs')

binOp ::  (Double -> Double -> Double) -> XlEvaluator -> XlValues
          -> XlFormula -> XlFormula -> (XlValue, XlValues)
binOp op ev vs a b =
   let
      (va, vs')   = toNumber $ (eScalar ev) ev vs  a
      (vb, vs'')  = toNumber $ (eScalar ev) ev vs' b
      
      doOp (XlNumber na)  (XlNumber nb)   = XlNumber (op na nb)
      doOp _              _               = XlError "#VALUE!"
      
      v = checkErr doOp va vb
   in
      (v, vs'')
\end{code}

\subsection{Type conversions}
\label{typeconv}

We conclude the presentation of the interpreter with the remaining
utility functions that perform various type conversions and checks.

Function |num2str| is a converter that presents rational and integer values
in their preferred notation (that is, with and without a decimal point,
respectively). Function |bool2num| converts booleans to 0 or 1.

\begin{code}
num2str :: Double -> String
num2str n = if fromIntegral (floor n) /= n then show n else show (floor n)

bool2num :: Bool -> Double
bool2num b = if b == True then 1 else 0
\end{code}

Functions |toNumber|, |toString| and |toBool| attempt to convert a value to
the specified type, producing a |XlError| value if the input is not
convertible.

\begin{code}
toNumber :: (XlValue, XlValues) -> (XlValue, XlValues)
toNumber (v, vs) = (coerce v, vs)
   where
      coerce (XlString s)  = case reads s :: [(Double, String)] of
                             []       -> XlError "#VALUE!"
                             [(n,_)]  -> XlNumber n
      coerce (XlBool b)    = XlNumber (bool2num b)
      coerce (XlEmpty)     = XlNumber 0
      coerce (XlMatrix _)  = XlError "#VALUE!"
      coerce v             = v

toString :: (XlValue, XlValues) -> (XlValue, XlValues)
toString (v, vs) = (coerce v, vs)
   where
      coerce (XlNumber n)  = XlString (num2str n)
      coerce (XlBool b)    = XlString (if b == True then "1" else "0")
      coerce (XlEmpty)     = XlString ""
      coerce (XlMatrix _)  = XlError "#VALUE!"
      coerce v             = v

toBool :: (XlValue, XlValues) -> (XlValue, XlValues)
toBool (v, vs) = (coerce v, vs)
   where
      coerce (XlNumber 0)  = XlBool False
      coerce (XlNumber _)  = XlBool True
      coerce (XlString s)  = case map toUpper s of
                             "TRUE"  -> XlBool True
                             "FALSE" -> XlBool False
                             _       -> XlError "#VALUE!"
      coerce (XlEmpty)     = XlBool False
      coerce (XlMatrix _)  = XlError "#VALUE!"
      coerce v             = v
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

%END LYX TEXT
\end{document}
