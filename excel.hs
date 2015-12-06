
import Data.Map.Strict as Map (Map, empty, elems, mapWithKey)
import Data.Graph as Graph (Graph, Vertex, graphFromEdges, transposeG, reachable)
import Data.List (foldl')
import Data.Tree (flatten)
import Data.Set as Set (Set, empty, union, toList, singleton, fromList)

data XlValue = XlNumber Double
             | XlString String
             | XlBoolean Bool
             | XlError String
   deriving Show

data XlAddr = XlAbs Integer
            | XlRel Integer
   deriving (Eq, Ord)

instance Show XlAddr where
   show (XlAbs n) = show n
   show (XlRel n) = "[" ++ show n ++ "]"

data XlRC = XlRC XlAddr XlAddr
   deriving (Eq, Ord)

instance Show XlRC where
   show (XlRC r c) = "R" ++ show r ++ "C" ++ show c

data XlFormulaAst = XlVal XlValue
                  | XlFun String [XlFormulaAst]
                  | XlRng XlRC XlRC
                  | XlRef XlRC
   deriving Show

data XlCell = XlFormula XlFormulaAst
            | XlArrayFormula XlFormulaAst
            | XlCellValue XlValue
   deriving Show

data XlWorksheet = XlWorksheet (Map.Map XlRC XlCell)
   deriving Show

data XlGraph = Graph

--data XlWorkbook = XlWorkbook (Map String XlWorksheet)

data XlEvent = XlEvent XlRC XlCell
   deriving Show

data XlEnv = XlEnv XlWorksheet (Map.Map XlRC XlValue)
   deriving Show

toAbs :: XlRC -> XlRC -> XlRC
toAbs base@(XlRC (XlAbs br) (XlAbs bc)) rc@(XlRC (XlAbs r) (XlAbs c)) = rc
toAbs base@(XlRC (XlAbs br) (XlAbs bc)) rc@(XlRC (XlAbs r) (XlRel c)) = XlRC (XlAbs r)        (XlAbs (bc + c))
toAbs base@(XlRC (XlAbs br) (XlAbs bc)) rc@(XlRC (XlRel r) (XlAbs c)) = XlRC (XlAbs (br + r)) (XlAbs c)
toAbs base@(XlRC (XlAbs br) (XlAbs bc)) rc@(XlRC (XlRel r) (XlRel c)) = XlRC (XlAbs (br + r)) (XlAbs (bc + c))
toAbs base@(XlRC (XlRel _) _) _ = error ("base in toAbs must be absolute, got " ++ show base)
toAbs base@(XlRC _ (XlRel _)) _ = error ("base in toAbs must be absolute, got " ++ show base)

rcsInAst :: XlRC -> XlFormulaAst -> Set XlRC
rcsInAst rc (XlVal _)      = Set.empty
rcsInAst rc (XlFun f asts) = foldl' (\rcs ast -> Set.union rcs (rcsInAst rc ast)) Set.empty asts
rcsInAst rc (XlRef ref)    = Set.singleton (toAbs rc ref)
rcsInAst rc (XlRng fr to)  = 
   let
      XlRC (XlAbs afr) (XlAbs afc) = toAbs rc fr
      XlRC (XlAbs atr) (XlAbs atc) = toAbs rc to
      (minR, maxR) = (min afr atr, max afr atr)
      (minC, maxC) = (min afc atc, max afc atc)
   in
      Set.fromList $ [ XlRC (XlAbs r) (XlAbs c) | r <- [minR..maxR], c <- [minC..maxC] ]      

sheetToNodes :: XlWorksheet -> [(XlCell, XlRC, [XlRC])]
sheetToNodes (XlWorksheet cells) =
   let
      convert :: XlRC -> XlCell -> (XlCell, XlRC, [XlRC])
      convert rc cell@(XlFormula ast) = (cell, rc, Set.toList $ rcsInAst rc ast)
      convert rc cell@(XlArrayFormula ast) = (cell, rc, Set.toList $ rcsInAst rc ast)
      convert rc cell@(XlCellValue _) = (cell, rc, [])
   in
      elems $ mapWithKey convert cells

run :: XlWorksheet -> [XlEvent] -> XlEnv
run sheet events =
   let
      runEvent :: XlEnv -> XlEvent -> XlEnv
      runEvent env@(XlEnv sheet values) event@(XlEvent rc newCell) =
         let
            (dependencyGraph, vertList, keyToVert) = graphFromEdges (sheetToNodes sheet)
            dependentsGraph :: Graph
            dependentsGraph = transposeG dependencyGraph
            affectedNodes :: [Vertex]
            affectedNodes =
               case keyToVert rc
               of Just v  -> reachable dependentsGraph v
                  Nothing -> []
         in
            env -- TODO
   
   in foldl' runEvent (XlEnv sheet Map.empty) events

main :: IO ()
main = print (run (XlWorksheet Map.empty) [])

