
import Data.Map.Strict as Map (Map, empty, elems, mapWithKey, insert, lookup)
import Data.Graph as Graph (Graph, Vertex, graphFromEdges, transposeG, reachable)
import Data.List (foldl')
import Data.Tree (flatten)
import Data.Char (ord, chr)
import Data.Set as Set (Set, empty, union, toList, singleton, fromList)
import Debug.Trace

data XlValue = XlNumber Double
             | XlString String
             | XlBoolean Bool
             | XlError String
   deriving Show

data XlAddr = XlAbs Int
            | XlRel Int
   deriving (Eq, Ord)

instance Show XlAddr where
   show (XlAbs n) = show n
   show (XlRel n) = "[" ++ show n ++ "]"

data XlRC = XlRC XlAddr XlAddr
   deriving (Eq, Ord)

instance Show XlRC where
   show (XlRC r@(XlAbs rn) c@(XlAbs cn)) = "R" ++ show r ++ "C" ++ show c ++ "(" ++ [chr (rn + 65)] ++ show (cn + 1) ++ ")"
   show (XlRC r c) = "R" ++ show r ++ "C" ++ show c ++ "(" ++ ")"

data XlFormulaRArg = XlRVal XlValue
                   | XlRMatrix [[XlValue]]
   deriving Show

data XlFormulaArg = XlAVal XlFormulaVal
                  | XlARng XlRC XlRC
   deriving Show

data XlFormulaVal = XlVal XlValue
                  | XlFun String [XlFormulaArg]
                  | XlRef XlRC
   deriving Show

data XlCell = XlFormula XlFormulaVal
--            | XlArrayFormula XlFormulaAst
   deriving Show

type XlCells = Map.Map XlRC XlCell

data XlWorksheet = XlWorksheet XlCells
   deriving Show

data XlGraph = Graph

--data XlWorkbook = XlWorkbook (Map String XlWorksheet)

data XlEvent = XlEvent XlRC XlCell
   deriving Show

type XlValues = Map.Map XlRC XlValue

data XlEnv = XlEnv XlCells XlValues
   deriving Show

toAbs :: XlRC -> XlRC -> XlRC
toAbs base@(XlRC br bc) cell@(XlRC cr cc) = XlRC (toAbsAddr br cr) (toAbsAddr bc cc)
   where 
      toAbsAddr :: XlAddr -> XlAddr -> XlAddr
      toAbsAddr _ a@(XlAbs _) = a
      toAbsAddr (XlAbs aa) (XlRel rr) = XlAbs (aa + rr)
      toAbsAddr base@(XlRel _) _ = error ("base in toAbs must be absolute, got " ++ show base) 

rcsInFArg rc (XlARng fr to)  = 
   let
      XlRC (XlAbs afr) (XlAbs afc) = toAbs rc fr
      XlRC (XlAbs atr) (XlAbs atc) = toAbs rc to
      (minR, maxR) = (min afr atr, max afr atr)
      (minC, maxC) = (min afc atc, max afc atc)
   in
      Set.fromList $ [ XlRC (XlAbs r) (XlAbs c) | r <- [minR..maxR], c <- [minC..maxC] ]      
rcsInFArg rc (XlAVal val)  = 
   rcsInFVal rc val

rcsInFVal :: XlRC -> XlFormulaVal -> Set XlRC
rcsInFVal rc (XlVal _)      = Set.empty
rcsInFVal rc (XlFun f args) = foldl' (\rcs arg -> Set.union rcs (rcsInFArg rc arg)) Set.empty args
rcsInFVal rc (XlRef ref)    = Set.singleton (toAbs rc ref)

cellsToNodes :: XlCells -> [(XlCell, XlRC, [XlRC])]
cellsToNodes cells =
   let
      convert :: XlRC -> XlCell -> (XlCell, XlRC, [XlRC])
      convert rc cell@(XlFormula fval) = (cell, rc, Set.toList $ rcsInFVal rc fval)
--      convert rc cell@(XlArrayFormula ast) = (cell, rc, Set.toList $ rcsInAst rc ast)
   in
      elems $ mapWithKey convert cells

evalFArg :: XlValues -> XlRC -> XlFormulaArg -> XlFormulaRArg
evalFArg values rcHere (XlAVal val) = XlRVal (evalFVal values rcHere val)
evalFArg values rcHere (XlARng from to) = XlRMatrix [[XlNumber 0]] -- TODO

evalFun :: String -> [XlFormulaRArg] -> XlValue
evalFun "+" [XlRVal (XlNumber a), XlRVal (XlNumber b)] = XlNumber (a + b)

evalFVal _ _ (XlVal value) = value
evalFVal values rcHere (XlRef rcRef) = 
   case (Map.lookup (toAbs rcHere rcRef) values) of
      Just v  -> v
      Nothing -> XlNumber 0
evalFVal values rcHere (XlFun name args) =
   let
      argVals :: [XlFormulaRArg]
      argVals = map (evalFArg values rcHere) args
   in
      evalFun name argVals

evalCell :: XlValues -> XlRC -> XlCell -> XlValue
evalCell values rcHere (XlFormula fval) = evalFVal values rcHere fval
-- evalCell values rcHere (XlArrayFormula fval) = -- TODO

updateCell values rc cell = insert rc (evalCell values rc cell) values

run :: XlWorksheet -> [XlEvent] -> XlEnv
run sheet@(XlWorksheet cells) events =
   let
      runEvent :: XlEnv -> XlEvent -> XlEnv
      runEvent env@(XlEnv cells values) event@(XlEvent rc newCell) =
         let
            (dependencyGraph, vertList, keyToVert) = graphFromEdges (cellsToNodes cells)
            
            dependentsGraph :: Graph
            dependentsGraph = transposeG dependencyGraph
            
            affectedNodes :: [Vertex]
            affectedNodes =
               case trace (show dependencyGraph) keyToVert rc of
                  Just v  -> filter (\e -> e /= v) (reachable dependentsGraph v)
                  Nothing -> []
               
            evalVertex :: XlEnv -> Vertex -> XlEnv
            evalVertex env@(XlEnv cells values) vtx =
               let
                  (cell, rc, _) = vertList vtx
               in
                  XlEnv cells (updateCell values rc cell) 

         in
            foldl evalVertex (XlEnv (insert rc newCell cells) (updateCell values rc newCell)) (trace (show affectedNodes) affectedNodes)
   
   in foldl' runEvent (XlEnv cells Map.empty) events

-- Converts Excel addresses in "A1" format to internal RC format.
-- Supports only rows A-Z, and absolute addresses.
toRC :: String -> XlRC
toRC (l:num) = XlRC (XlAbs ((ord l) - 65)) (XlAbs ((read num) - 1))

main :: IO ()
main = print $ run (XlWorksheet Map.empty)
                  [
                   (XlEvent (toRC "A1") (XlFormula (XlVal (XlNumber 15)))),
                   (XlEvent (toRC "B1") (XlFormula (XlVal (XlNumber 0)))),
                   (XlEvent (toRC "A2") (XlFormula (XlFun "+" [XlAVal (XlRef (toRC "A1")), XlAVal (XlRef (toRC "B1"))]))),
                   (XlEvent (toRC "B1") (XlFormula (XlRef (toRC "A1"))))
                  ]

