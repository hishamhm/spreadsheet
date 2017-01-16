
import Data.Map.Strict as Map (Map, empty, elems, mapWithKey, foldrWithKey, member, insert, lookup, toList)
import Data.Graph as Graph (Graph, Vertex, graphFromEdges, transposeG, reachable)
import Data.List (foldl')
import Data.Tree (flatten)
import Data.Char (ord, chr)
import Data.Set as Set (Set, insert, member, empty, union, toList, singleton, fromList)
import Debug.Trace
import Data.Fixed
import Text.PrettyPrint.Boxes as Box (render, hcat, vcat, text)
import Text.PrettyPrint.Boxes as Alignment (left, right)

data XlValue = XlNumber Double
             | XlString String
             | XlBoolean Bool
             | XlError String
             | XlMatrix [[XlValue]]
   deriving Eq

instance Show XlValue where
   show (XlNumber d)  = show d
   show (XlString s)  = show s
   show (XlBoolean b) = show b
   show (XlError e)   = show e
   show (XlMatrix m)  = show m

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

data XlFormula = XlLit XlValue
               | XlFun String [XlFormula]
               | XlRef XlRC
               | XlRng XlRC XlRC
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

type XlDeps = [(XlCell, XlRC, [XlRC])]

data XlEnv = XlEnv XlCells XlValues XlDeps

instance Show XlEnv where
   show (XlEnv cells values _) = (Box.render $ Box.vcat Alignment.left $ map Box.text $ (map show (Map.toList cells))) ++ "\n\n" ++ (Box.render $ Box.hcat Alignment.left $ numbers : map doRow [0..25])
      where
         lpad m xs = reverse $ take m $ reverse $ (take m $ repeat ' ') ++ (take m xs)
         numbers = Box.vcat Alignment.right $ map Box.text $ " " : map show [1..26]
         doRow r = Box.vcat Alignment.left $ Box.text ['|', chr (r + 65)] : map doColumn [0..25]
            where
               doColumn c =
                  let
                     rc = (XlRC (XlAbs r) (XlAbs c))
                     val = Map.lookup rc values
                  in
                     case val of
                        Just (XlNumber n)  -> Box.text ('|' : (lpad 11 (toString n)))
                        Just v  -> Box.text ('|' : show v)
                        Nothing -> Box.text "|"

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

toString :: Double -> String
toString n =
      if fromIntegral (floor n) /= n
      then show n
      else show (floor n)

putValue :: XlEnv -> XlRC -> XlValue -> XlEnv
putValue env@(XlEnv cells values deps) rc value = XlEnv cells (Map.insert rc value values) deps

evalFormula :: Set XlRC -> XlEnv -> XlRC -> XlFormula -> (XlValue, XlEnv)
evalFormula _ env rc (XlLit value) = (value, putValue env rc value)

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

evalFormula visiting cells values rc (XlRng from to) = 
   let
      (XlRC (XlAbs fromR) (XlAbs fromC)) = toAbs rc from
      (XlRC (XlAbs toR)   (XlAbs toC))   = toAbs rc to
      minR = min fromR toR
      maxR = max fromR toR
      minC = min fromC toC
      maxC = max fromC toC
      (mtx, newValues) =
         foldl' handleRow ([], values) [minR..maxR]
            where
               handleRow :: ([[XlValue]], XlValues) -> Int -> ([[XlValue]], XlValues)
               handleRow (curMtx, curValues) r =
                  let
                     (newRow, newValues) =
                        foldl' handleValue ([], curValues) [minC..maxC]
                           where
                              handleValue (curRow, curRowValues) c =
                                 let
                                    curRc = XlRC (XlAbs r) (XlAbs c)
                                    (newValue, newRowValues) = evalFormula visiting cells curRowValues curRc (XlRef curRc)
                                 in
                                    (curRow ++ [newValue], newRowValues)
                  in
                     (curMtx ++ [newRow], newValues)
   in
      (XlMatrix mtx, newValues)

evalFormula visiting cells values rc (XlFun "IF" [i, t, e]) =
   let
      (vi, valuesi) = evalFormula (Set.insert rc visiting) cells values  rc i
      (vt, valuest) = evalFormula (Set.insert rc visiting) cells valuesi rc t
      (ve, valuese) = evalFormula (Set.insert rc visiting) cells valuesi rc e
      (vr, valuesr) = 
         case vi of
            err@(XlError _) -> (vi, valuesi)
            (XlNumber 0)    -> (ve, valuese)
            (XlMatrix _)    -> ((XlError "#VALUE!"), valuesi)
            _               -> (vt, valuest)
   in
      (vr, Map.insert rc vr valuesr)

evalFormula visiting cells values rc (XlFun "INDIRECT" [addr]) =
   let
      (va, valuesa) = evalFormula (Set.insert rc visiting) cells values rc addr
      (vr, valuesr) = 
         case va of
            XlString computed ->
               evalFormula (Set.insert rc visiting) cells valuesa rc (XlRef (toRC computed))
            _ ->
               ((XlError "#VALUE!"), valuesa)
   in
      (vr, Map.insert rc vr valuesr)

evalFormula visiting cells values rc (XlFun "+" [a, b]) =
   let
      (va, values')  = evalFormula (Set.insert rc visiting) cells values  rc a
      (vb, values'') = evalFormula (Set.insert rc visiting) cells values' rc b
      
      doSum e@(XlError _) _             = e
      doSum _             e@(XlError _) = e
      doSum (XlNumber na) (XlNumber nb) = XlNumber (na + nb)
      doSum (XlString sa) (XlNumber nb) = XlString (sa ++ (toString nb))
      doSum (XlNumber na) (XlString sb) = XlString ((toString na) ++ sb)
      doSum (XlString sa) (XlString sb) = XlString (sa ++ sb)
      doSum _             _             = XlError "#VALUE!"
      
      val = doSum va vb
   in
      (val, Map.insert rc val values'')


evalFormula visiting cells values rc (XlFun "SUM" [rng]) =
   let
      (vrng, values') = evalFormula (Set.insert rc visiting) cells values rc rng
      
      doSum e@(XlError _) _            = e
      doSum _ e@(XlError _)            = e
      doSum s@(XlString _) v           = v
      doSum v s@(XlString _)           = v
      doSum (XlBoolean b) (XlNumber n) = XlNumber ((if b == True then 1 else 0) + n)
      doSum (XlNumber n) (XlBoolean b) = XlNumber ((if b == True then 1 else 0) + n)
      doSum (XlNumber a) (XlNumber b)  = XlNumber (a + b)
      
      val = 
         case vrng of
            XlMatrix mtx ->
               foldl' (foldl' doSum) (XlNumber 0) mtx
            _ ->
               XlError "#VALUE!"
   in
      (val, Map.insert rc val values')

updateCell :: XlEnv -> XlRC -> XlCell -> XlEnv
updateCell env@(XlEnv cells values deps) rc cell@(XlCell formula) =
   let
      newCells = (Map.insert rc cell cells)
      env' = XlEnv newCells values deps
      (_, (newValues, newDeps)) = evalFormula Set.empty env' rc formula
   in
      XlEnv newCells newValues newDeps

calcCell env rc (XlAFCell formula) = undefined -- TODO

run :: XlWorksheet -> [XlEvent] -> XlEnv
run sheet@(XlWorksheet cells) events =
   let
      runEvent :: XlEnv -> XlEvent -> XlEnv
      runEvent env@(XlEnv cells values deps) event@(XlEvent rc newCell) =
         updateCell (XlEnv cells values deps) rc newCell
 
   in foldl' runEvent (XlEnv cells Map.empty []) events

str :: String -> XlFormula
str s = XlLit (XlString s)

num :: Double -> XlFormula
num n = XlLit (XlNumber n)

cell :: String -> XlFormula
cell a1 = XlRef (toRC a1)

range :: String -> String -> XlFormula
range a1 b2 = XlRng (toRC a1) (toRC b2)

main :: IO ()
main = print $ run (XlWorksheet Map.empty)
                  [
                   (XlEvent (toRC "A1") (XlCell (num 15))),
                   (XlEvent (toRC "B1") (XlCell (num 0))),
                   (XlEvent (toRC "A2") (XlCell (XlFun "+" [cell "A1", cell "B1"]))),
                   (XlEvent (toRC "B1") (XlCell (cell "A1"))),
                   (XlEvent (toRC "C1") (XlCell (str "B"))),
                   (XlEvent (toRC "C2") (XlCell (num 1))),
                   (XlEvent (toRC "B2") (XlCell (XlFun "INDIRECT" [XlFun "+" [cell "C1", cell "C2"]]))),
                   (XlEvent (toRC "D1") (XlCell (XlFun "SUM" [range "A1" "B2"]) )),
                   (XlEvent (toRC "E1") (XlCell (XlFun "SUM" [range "B1" "B2"]) )),
                   (XlEvent (toRC "F1") (XlCell (XlFun "SUM" [range "D1" "E1"]) ))
                  ]
