
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
   show (XlRC r@(XlAbs rn) c@(XlAbs cn)) = "<" ++ [chr (rn + 65)] ++ show (cn + 1) ++ ">"
   show (XlRC r c) = "R" ++ show r ++ "C" ++ show c

data XlArgValue = XlVal XlValue
                | XlMtx [[XlValue]]
   deriving Show

data XlArg = XlFml XlFormula
           | XlRng XlRC XlRC
   deriving Show

data XlFormula = XlLit XlValue
               | XlFun String [XlArg]
               | XlRef XlRC
   deriving Show

data XlCell = XlCell XlFormula
            | XlAFCell XlFormula
   deriving Show

type XlCells = Map.Map XlRC XlCell

data XlWorksheet = XlWorksheet XlCells
   deriving Show

--data XlWorkbook = XlWorkbook (Map String XlWorksheet)

data XlEvent = XlEvent XlRC XlCell
   deriving Show

type XlValues = Map.Map XlRC XlValue

data XlState = XlState XlCells XlValues
   deriving Show

toAbs :: XlRC -> XlRC -> XlRC
toAbs base@(XlRC br bc) cell@(XlRC cr cc) = XlRC (toAbsAddr br cr) (toAbsAddr bc cc)
   where 
      toAbsAddr :: XlAddr -> XlAddr -> XlAddr
      toAbsAddr _ a@(XlAbs _) = a
      toAbsAddr (XlAbs aa) (XlRel rr) = XlAbs (aa + rr)
      toAbsAddr base@(XlRel _) _ = error ("base in toAbs must be absolute, got " ++ show base) 

rcsInArg rc (XlRng fr to)  = 
   let
      XlRC (XlAbs afr) (XlAbs afc) = toAbs rc fr
      XlRC (XlAbs atr) (XlAbs atc) = toAbs rc to
      (minR, maxR) = (min afr atr, max afr atr)
      (minC, maxC) = (min afc atc, max afc atc)
   in
      Set.fromList $ [ XlRC (XlAbs r) (XlAbs c) | r <- [minR..maxR], c <- [minC..maxC] ]      
rcsInArg rc (XlFml val)  = 
   rcsInFormula rc val

rcsInFormula :: XlRC -> XlFormula -> Set XlRC
rcsInFormula rc (XlLit _)      = Set.empty
rcsInFormula rc (XlFun f args) = foldl' (\rcs arg -> Set.union rcs (rcsInArg rc arg)) Set.empty args
rcsInFormula rc (XlRef ref)    = Set.singleton (toAbs rc ref)

cellsToNodes :: XlCells -> [(XlCell, XlRC, [XlRC])]
cellsToNodes cells =
   let
      convert :: XlRC -> XlCell -> (XlCell, XlRC, [XlRC])
      convert rc cell@(XlCell formula) = (cell, rc, Set.toList $ rcsInFormula rc formula)
--      convert rc cell@(XlArrayFormula ast) = (cell, rc, Set.toList $ rcsInAst rc ast)
   in
      Map.elems $ mapWithKey convert cells

evalArg :: XlValues -> XlRC -> XlArg -> XlArgValue
evalArg values rc (XlFml val) = XlVal (evalFormula values rc val)
evalArg values rc (XlRng from to) = XlMtx [[XlNumber 0]] -- TODO

evalFun :: String -> [XlArgValue] -> XlValue
evalFun "+" [XlVal (XlNumber a), XlVal (XlNumber b)] = XlNumber (a + b)

evalFormula _ _ (XlLit value) = value
evalFormula values rc (XlRef rcRef) = 
   case (Map.lookup (toAbs rc rcRef) values) of
      Just v  -> v
      Nothing -> XlNumber 0
evalFormula values rc (XlFun name args) =
   let
      argVals :: [XlArgValue]
      argVals = map (evalArg values rc) args
   in
      evalFun name argVals

evalCell :: XlValues -> XlRC -> XlCell -> XlValue
evalCell values rc (XlCell formula) = evalFormula values rc formula
-- evalCell values rc (XlArrayFormula formula) = -- TODO

updateValue rc cell values = insert rc (evalCell values rc cell) values

run :: XlWorksheet -> [XlEvent] -> XlState
run sheet@(XlWorksheet cells) events =
   foldl' runEvent (XlState cells Map.empty) events
   where
      runEvent :: XlState -> XlEvent -> XlState
      runEvent env@(XlState cells values) event@(XlEvent rc newCell) =
         let
            (dependencyGraph, vertList, keyToVert) = graphFromEdges (cellsToNodes cells)
            
            dependentsGraph :: Graph
            dependentsGraph = transposeG dependencyGraph
            
            affectedNodes :: [Vertex]
            affectedNodes =
               case trace (show dependencyGraph) keyToVert rc of
                  Just v  -> filter (\e -> e /= v) (reachable dependentsGraph v)
                  Nothing -> []
            
            evalVertex :: XlState -> Vertex -> XlState
            evalVertex env@(XlState cells values) vtx =
               let
                  (cell, rc, _) = vertList vtx
               in
                  XlState cells (updateValue rc cell values)
         in
            foldl evalVertex (XlState (insert rc newCell cells) (updateValue rc newCell values)) (trace (show affectedNodes) affectedNodes)

-- Converts Excel addresses in "A1" format to internal RC format.
-- Supports only rows A-Z, and absolute addresses.
toRC :: String -> XlRC
toRC (l:num) = XlRC (XlAbs ((ord l) - 65)) (XlAbs ((read num) - 1))

main :: IO ()
main = print $ run (XlWorksheet Map.empty)
                  [
                   (XlEvent (toRC "A1") (XlCell (XlLit (XlNumber 15)))),
                   (XlEvent (toRC "B1") (XlCell (XlLit (XlNumber 0)))),
                   (XlEvent (toRC "A2") (XlCell (XlFun "+" [XlFml (XlRef (toRC "A1")), XlFml (XlRef (toRC "B1"))]))),
                   (XlEvent (toRC "B1") (XlCell (XlRef (toRC "A1"))))
                  ]
