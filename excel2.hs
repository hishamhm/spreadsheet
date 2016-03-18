
import Data.Map.Strict as Map (Map, empty, elems, mapWithKey, foldrWithKey, member, insert, lookup)
import Data.Graph as Graph (Graph, Vertex, graphFromEdges, transposeG, reachable)
import Data.List (foldl')
import Data.Tree (flatten)
import Data.Char (ord, chr)
import Data.Set as Set (Set, insert, member, empty, union, toList, singleton, fromList)
import Debug.Trace
import Data.Fixed

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

data XlEnv = XlEnv XlCells XlValues
   deriving Show

toAbs :: XlRC -> XlRC -> XlRC
toAbs base@(XlRC br bc) cell@(XlRC cr cc) = XlRC (toAbsAddr br cr) (toAbsAddr bc cc)
   where 
      toAbsAddr :: XlAddr -> XlAddr -> XlAddr
      toAbsAddr _ a@(XlAbs _) = a
      toAbsAddr (XlAbs aa) (XlRel rr) = XlAbs (aa + rr)
      toAbsAddr base@(XlRel _) _ = error ("base in toAbs must be absolute, got " ++ show base) 

-- Converts Excel addresses in "A1" format to internal RC format.
-- Supports only rows A-Z, and absolute addresses.
toRC :: String -> XlRC
toRC (l:num) = XlRC (XlAbs ((ord l) - 65)) (XlAbs ((read num) - 1))

evalArg :: Set XlRC -> XlCells -> XlValues -> XlRC -> XlArg -> (XlArgValue, XlValues)
evalArg visiting cells values rc (XlFml formula) = (XlVal newValue, newValues) where (newValue, newValues) = evalFormula (Set.insert rc visiting) cells values rc formula
evalArg visiting cells values rc (XlRng from to) = (XlMtx [[XlNumber 0]], values) -- TODO

toString :: Double -> String
toString n =
   if (toInteger n) /= n
   then show n
   else show (toInteger n)

evalFormula :: Set XlRC -> XlCells -> XlValues -> XlRC -> XlFormula -> (XlValue, XlValues)
evalFormula _        _     values rc (XlLit value) = (value, Map.insert rc value values)

evalFormula visiting cells values rc (XlRef ref')  = 
   let
      ref = toAbs rc ref'
      give val = (val, Map.insert rc val values)
   in
      if Set.member ref visiting
      then give (XlError "#LOOP!")
      else 
         case Map.lookup ref values of
            Just v  -> give v
            Nothing ->
               case Map.lookup ref cells of
                  Just (XlCell formula) -> evalFormula (Set.insert rc visiting) cells values ref formula
                  Nothing               -> give (XlNumber 0)

evalFormula visiting cells values rc (XlFun "+" [a, b]) =
   let
      (va, values')  = evalArg visiting cells values  rc a
      (vb, values'') = evalArg visiting cells values' rc b
      
      doSum (XlVal e@(XlError _)) _                     = e
      doSum _                     (XlVal e@(XlError _)) = e
      doSum (XlVal (XlNumber na)) (XlVal (XlNumber nb)) = XlNumber (na + nb)
      doSum (XlVal (XlString sa)) (XlVal (XlNumber nb)) = XlString (sa ++ (toString nb))
      doSum (XlVal (XlNumber na)) (XlVal (XlString sb)) = XlString ((toString na) ++ sb)
      doSum (XlVal (XlString sa)) (XlVal (XlString sb)) = XlString (sa ++ sb)
      doSum _                    _                      = XlError "#VALUE!"
      val = doSum va vb
   in
      (val, Map.insert rc val values'')

evalFormula visiting cells values rc (XlFun "IF" [i, t, e]) =
   let
      (vi, valuesi) = evalArg visiting cells values  rc i
      (vt, valuest) = evalArg visiting cells valuesi rc t
      (ve, valuese) = evalArg visiting cells valuesi rc e
      (XlVal vr, valuesr) = 
         case vi of
            XlVal err@(XlError _) -> (vi, valuesi)
            XlVal (XlNumber 0)    -> (ve, valuese)
            XlVal _               -> (vt, valuest)
            _                     -> (XlVal (XlError "#VALUE!"), valuesi)
   in
      (vr, Map.insert rc vr valuesr)

evalFormula visiting cells values rc (XlFun "INDIRECT" [addr]) =
   let
      (va, valuesa) = evalArg visiting cells values rc addr
      (XlVal vr, valuesr) = 
         case va of
            XlVal (XlString computed) ->
               evalArg visiting cells valuesa rc (XlFml (XlRef (toRC computed)))
            _ ->
               (XlVal (XlError "#VALUE!"), valuesa)
   in
      (vr, Map.insert rc vr valuesr)

calcCell :: Set XlRC -> XlCells -> XlValues -> XlRC -> XlCell -> XlValues
calcCell visiting cells values rc (XlCell formula) = newValues where (_, newValues) = evalFormula visiting cells values rc formula
{-
calcCell visiting cells values rc (XlAFCell formula)                     = -- TODO
-}

run :: XlWorksheet -> [XlEvent] -> XlEnv
run sheet@(XlWorksheet cells) events =
   let
      runEvent :: XlEnv -> XlEvent -> XlEnv
      runEvent env@(XlEnv cells _) event@(XlEvent rc newCell) =
         let 
            newCells = Map.insert rc newCell cells
            
            acc :: XlRC -> XlCell -> XlValues -> XlValues
            acc rc cell values =
               if Map.member rc values
               then values
               else calcCell Set.empty newCells values rc cell
         in
            XlEnv newCells (Map.foldrWithKey acc Map.empty newCells)
 
   in foldl' runEvent (XlEnv cells Map.empty) events

str :: String -> XlFormula
str s = XlLit (XlString s)

num :: Double -> XlFormula
num n = XlLit (XlNumber n)

cell :: String -> XlFormula
cell a1 = XlRef (toRC a1)

fun :: String -> [XlFormula] -> XlFormula
fun name args = XlFun name (map XlFml args)

main :: IO ()
main = print $ run (XlWorksheet Map.empty)
                  [
                   (XlEvent (toRC "A1") (XlCell (num 15))),
                   (XlEvent (toRC "B1") (XlCell (num 0))),
                   (XlEvent (toRC "A2") (XlCell (fun "+" [cell "A1", cell "B1"]))),
                   (XlEvent (toRC "B1") (XlCell (cell "A1"))),
                   (XlEvent (toRC "C1") (XlCell (str "B"))),
                   (XlEvent (toRC "C2") (XlCell (num 1))),
                   (XlEvent (toRC "B2") (XlCell (fun "INDIRECT" [fun "+" [cell "C1", cell "C2"]])))
                  ]
