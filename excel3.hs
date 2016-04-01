
import Data.Map.Strict as Map (Map, empty, elems, mapWithKey, foldrWithKey, member, insert, lookup, toList, (!))
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
   show (XlNumber d)  = toString d
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
            | XlAFCell XlFormula (Int, Int)
   deriving Show

type XlCells = Map.Map XlRC XlCell

data XlWorksheet = XlWorksheet XlCells
   deriving Show

--data XlWorkbook = XlWorkbook (Map String XlWorksheet)

data XlEvent = XlAddFormula XlRC XlFormula
             | XlAddArrayFormula XlRC XlRC XlFormula
   deriving Show

type XlValues = Map.Map XlRC XlValue

data XlEnv = XlEnv XlCells XlValues

instance Show XlEnv where
   show (XlEnv cells values) = (Box.render $ Box.vcat Alignment.left $ map Box.text $ (map show (Map.toList cells))) ++ "\n\n" ++ (Box.render $ Box.hcat Alignment.left $ numbers : map doRow [0..25])
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

--foldRange :: XlRC -> XlRC -> XlRC -> rowAcc -> (rowAcc -> cellAcc) -> (cellAcc -> XlRC -> cellAcc) -> (rowAcc -> Int -> cellAcc -> rowAcc) -> rowAcc
foldRange rc from to zero zeroRow cellOp rowOp =
   let
      (XlRC (XlAbs fromR) (XlAbs fromC)) = toAbs rc from
      (XlRC (XlAbs toR)   (XlAbs toC))   = toAbs rc to
      minR = min fromR toR
      maxR = max fromR toR
      minC = min fromC toC
      maxC = max fromC toC
      --handleRow :: rowAcc -> Int -> rowAcc
      handleRow accRow r = rowOp accRow r rowRes
         where
            --handleCell :: cellAcc -> Int -> cellAcc
            handleCell accCell c = cellOp accCell (XlRC (XlAbs r) (XlAbs c))
            rowRes = foldl' handleCell (zeroRow accRow) [minC..maxC]
   in
      foldl' handleRow zero [minR..maxR]

litValue (XlLit value) = value
litValue _             = XlError "#VALUE!"

bool2num b = if b == True then 1 else 0

checkNumber (v, vs) = (coerce v, vs)
   where
      coerce n@(XlNumber _) = n
      coerce (XlString s) =
         case reads s :: [(Double, String)] of
            []      -> XlError "#VALUE!"
            [(n,_)] -> XlNumber n
      coerce (XlBoolean b) = XlNumber (bool2num b)
      coerce x = x

checkString (v, vs) = (coerce v, vs)
   where
      coerce s@(XlString _) = s
      coerce (XlNumber n)  = XlString (toString n)
      coerce (XlBoolean b) = XlString (if b == True then "1" else "0")
      coerce x = x

data Evaluator = Evaluator {
   rc :: XlRC,
   visiting :: Set XlRC,
   array  :: Evaluator -> XlValues -> XlFormula -> (XlValue, XlValues),
   scalar :: Evaluator -> XlValues -> XlFormula -> (XlValue, XlValues)
}

instance Show Evaluator where
   show ev = "Evaluator\nRC: " ++ show (rc ev) ++ "\nVisiting: " ++ show (visiting ev) ++ "\n"

evalFormula :: Evaluator -> XlValues -> XlFormula -> (XlValue, XlValues)

evalFormula ev values (XlRng from to) = 
   makeXlMatrix $ foldRange (rc ev) from to ([], values) zeroRow cellOp rowOp
      where
         makeXlMatrix (mtx, values) = (XlMatrix mtx, values)
      
         zeroRow :: ([[XlValue]], XlValues) -> ([XlValue], XlValues)
         zeroRow (_, curValues) = ([], curValues)

         cellOp :: ([XlValue], XlValues) -> XlRC -> ([XlValue], XlValues)
         cellOp (curRow, curRowValues) curRc =
            addToRow $ (scalar ev) ev curRowValues (XlRef curRc)
               where addToRow (newValue, newRowValues) = (curRow ++ [newValue], newRowValues)

         rowOp :: ([[XlValue]], XlValues) -> Int -> ([XlValue], XlValues) -> ([[XlValue]], XlValues)
         rowOp (curMtx, _) r (newRow, newValues) = (curMtx ++ [newRow], newValues)

evalFormula ev values (XlFun "IF" [i, t, e]) =
   let
      (vi, valuesi) = checkNumber $ (scalar ev) ev values i
      (vr, valuesr) = 
         case vi of
            (XlError _)  -> (vi, valuesi)
            (XlNumber 0) -> (scalar ev) ev valuesi e
            (XlNumber _) -> (scalar ev) ev valuesi t
            _            -> ((XlError "#VALUE!"), valuesi)
   in
      (vr, Map.insert (rc ev) vr valuesr)

evalFormula ev values (XlFun "INDIRECT" [addr]) =
   let
      (va, valuesa) = checkString $ (scalar ev) ev values addr
      (vr, valuesr) = 
         case va of
            XlString computed -> (scalar ev) ev valuesa converted
                                    where
                                       (a1, b2) = break (== ':') computed
                                       converted = 
                                          case b2 of
                                             (':':b2') -> (XlRng (toRC a1) (toRC b2')) -- FIXME error checking
                                             _         -> (XlRef (toRC computed)) -- FIXME error checking
            _                 -> ((XlError "#VALUE!"), valuesa)
   in
      (vr, Map.insert (rc ev) vr valuesr)

evalFormula ev values (XlFun "ABS" [v]) =
   let
      (v', values') = checkNumber $ (scalar ev) ev values v
      
      doAbs (XlNumber n)  = XlNumber $ abs n
      doAbs _             = XlError "#VALUE!"
      
      val = doAbs v'
   in
      (val, Map.insert (rc ev) val values')

evalFormula ev values (XlFun "SUM" [rng]) =
   let
      (vrng, values') = (array ev) ev values rng
      
      doSum e@(XlError _)  _              = e
      doSum _              e@(XlError _)  = e
      doSum s@(XlString _) v              = v
      doSum v              s@(XlString _) = v
      doSum (XlBoolean b)  (XlNumber n)   = XlNumber (bool2num b + n)
      doSum (XlNumber n)   (XlBoolean b)  = XlNumber (bool2num b + n)
      doSum (XlNumber a)   (XlNumber b)   = XlNumber (a + b)
      
      val = 
         case vrng of
            XlMatrix mtx -> foldl' (foldl' doSum) (XlNumber 0) mtx
            _            -> XlError "#VALUE!"
   in
      (val, Map.insert (rc ev) val values')

evalFormula ev values (XlFun "+" [a, b]) =
   let
      (va, values')  = checkNumber $ (scalar ev) ev values  a
      (vb, values'') = checkNumber $ (scalar ev) ev values' b
      
      doSum e@(XlError _) _               = e
      doSum _             e@(XlError _)   = e
      doSum (XlNumber na) (XlNumber nb)   = XlNumber (na + nb)
      doSum _             _               = XlError "#VALUE!"
      
      val = doSum va vb
   in
      (val, Map.insert (rc ev) val values'')

evalFormula ev values (XlFun "&" [a, b]) =
   let
      (va, values')  = checkString $ (scalar ev) ev values  a
      (vb, values'') = checkString $ (scalar ev) ev values' b
      
      doConcat e@(XlError _) _               = e
      doConcat _             e@(XlError _)   = e
      doConcat (XlString sa) (XlString sb)   = XlString (sa ++ sb)
      doConcat _             _               = XlError "#VALUE!"
      
      val = doConcat va vb
   in
      (val, Map.insert (rc ev) val values'')

evalFormula ev values (XlFun "/" [a, b]) =
   let
      (va, values')  = checkNumber $ (scalar ev) ev values  a
      (vb, values'') = checkNumber $ (scalar ev) ev values' b
      
      doDiv e@(XlError _) _             = e
      doDiv _             e@(XlError _) = e
      doDiv (XlNumber na) (XlNumber 0)  = XlError "#DIV/0!"
      doDiv (XlNumber na) (XlNumber nb) = XlNumber (na / nb)
      doDiv _             _             = XlError "#VALUE!"
      
      val = doDiv va vb
   in
      (val, Map.insert (rc ev) val values'')

evalFormula ev values (XlFun "=" [a, b]) =
   let
      (va, values')  = (scalar ev) ev values  a
      (vb, values'') = (scalar ev) ev values' b
      
      doEq e@(XlError _)  _              = e -- in case of two errors, the first is propagated
      doEq _              e@(XlError _)  = e
      doEq (XlNumber na)  (XlNumber nb)  = XlBoolean (na == nb)
      doEq (XlString sa)  (XlString sb)  = XlBoolean (sa == sb)
      doEq (XlBoolean ba) (XlBoolean bb) = XlBoolean (ba == bb)
      doEq (XlNumber na)  (XlBoolean bb) = XlBoolean (na == bool2num bb)
      doEq (XlBoolean ba) (XlNumber nb)  = XlBoolean (bool2num ba == nb)
      doEq _              (XlString sb)  = XlBoolean False
      doEq (XlString sa)  _              = XlBoolean False
      doEq _              _              = undefined
      
      val = doEq va vb
   in
      (val, Map.insert (rc ev) val values'')

evalFormula ev va fo = trace ("[ev] "++ show ev ++"\n[va] "++ show va ++"\n[fo] "++ show fo++"\n") undefined

getRef :: Evaluator -> XlCells -> XlValues -> XlRC -> (XlValue, XlValues)
getRef ev cells values ref' =
   let
      ref = toAbs (rc ev) ref'
      give val = (val, Map.insert (rc ev) val values)
   in
      if Set.member ref (visiting ev)
      then give (XlError "#LOOP!")
      else 
         case Map.lookup ref values of
            Just v  -> give v
            Nothing ->
               case Map.lookup ref cells of
                  Just (XlCell formula) -> (scalar ev) (ev { rc = ref }) values formula
                  Nothing               -> give (XlNumber 0)

calcCell :: XlCells -> XlValues -> XlRC -> XlCell -> XlValues
calcCell cells values myRC@(XlRC (XlAbs r) (XlAbs c)) (XlCell formula) = snd $ (scalar ev) ev values formula
   where
   
      ev = Evaluator {
         rc = myRC,
         visiting = Set.empty,
         scalar = scalarEvaluator,
         array = arrayEvaluator
      }
      
      arrayEvaluator ev values formula =
         case formula of
            XlLit v -> (v, Map.insert (rc ev) v values)
            XlRef ref -> getRef ev cells values ref
            _ -> evalFormula (ev { visiting = Set.insert (rc ev) (visiting ev) }) values formula
         
      -- NOTE that we can't just evaluate a range to a matrix and convert to scalar,
      -- because explicit ranges are converted via intersection.
      scalarEvaluator ev values formula =
         let
            formula' = toScalar formula
         in
            case formula' of
               XlLit v -> (v, Map.insert (rc ev) v values)
               XlRef ref -> getRef ev cells values ref
               _ -> evalFormula (ev { visiting = Set.insert (rc ev) (visiting ev) }) values formula'

      -- 3.3) Non-Scalar Evaluation (aka 'Array expressions') [page 27]
      -- 1) Evaluation as an implicit intersection of the argument with the expression's evaluation position.
      -- 1.1) Inline Arrays
      -- Element (0;0) of the array is used in the place of the array.
      toScalar (XlLit (XlMatrix mtx)) = XlLit (head (head mtx))
      
      -- 1.2) References
      toScalar (XlRng rcFrom rcTo) =
         let
            XlRC (XlAbs fromR) (XlAbs fromC) = toAbs myRC rcFrom
            XlRC (XlAbs toR)   (XlAbs toC)   = toAbs myRC rcTo
         in
            -- 1.2.1) If the target reference is a row-vector (Nx1) use the value at the intersection of
            -- the evaluation position's column and the reference's row.
            if fromC == toC
            then
               if (r >= fromR) && (r <= toR)
               then XlRef (XlRC (XlAbs r) (XlAbs fromC))
               -- Note 2: If there is no intersection the result is #VALUE!
               else XlLit (XlError "#VALUE!")
            -- 1.2.2) If the target reference is a column-vector (1xM) the value at the intersection of
            -- the evaluation position's row and the reference's column.
            else if fromR == toR
            then
               if (c >= fromC) && (c <= toC)
               then XlRef (XlRC (XlAbs fromR) (XlAbs c))
               else XlLit (XlError "#VALUE!")
            else
               XlLit (XlError "#VALUE!")

      toScalar v = v

{-
calcCell visiting cells values rc (XlAFCell formula (y, x)) = snd $ (scalar ev) ev values rc formula
   where
   
      ev = Evaluator {
         scalar = scalarEvaluator,
         array = arrayEvaluator
      }
   
      matrixEvaluator visiting values rc formula =
         let
            formula' = sliceMatrix formula
         in
            case formula of
               XlLit v -> (v, Map.insert rc v values)
               _ -> evalFormula matrixEvaluator (Set.insert rc visiting) cells values rc formula'
               
      sliceMatrix (XlLit (XlMatrix mtx)) =
-}

updateCells cells event@(XlAddFormula rc formula) =
   Map.insert rc (XlCell formula) cells

updateCells cells event@(XlAddArrayFormula rcFrom rcTo formula) =
   fst $ foldRange rcFrom rcFrom rcTo (cells, (0, 0)) id cellOp rowOp
      where
         cellOp (cells, (y, x)) rc = (Map.insert rc (XlAFCell formula (y, x)) cells, (y, x + 1))
         rowOp _ r (cells, (y, x)) = (cells, (y + 1, 0))

run :: XlWorksheet -> [XlEvent] -> XlEnv
run sheet@(XlWorksheet cells) events =
   let
      runEvent :: XlEnv -> XlEvent -> XlEnv
      runEvent env@(XlEnv cells _) event =
         let 
            newCells = updateCells cells event
            
            acc :: XlRC -> XlCell -> XlValues -> XlValues
            acc rc cell values =
               if Map.member rc values
               then values
               else calcCell newCells values rc cell
         in
            XlEnv newCells (Map.foldrWithKey acc Map.empty newCells)

   in foldl' runEvent (XlEnv cells Map.empty) events

str :: String -> XlFormula
str s = XlLit (XlString s)

num :: Double -> XlFormula
num n = XlLit (XlNumber n)

cell :: String -> XlFormula
cell a1 = XlRef (toRC a1)

range :: String -> String -> XlFormula
range a1 b2 = XlRng (toRC a1) (toRC b2)

main :: IO ()
main = 
   let
      operations = [
            ( (XlAddFormula (toRC "A1") (num 15)),                                      XlNumber 15 ),
            ( (XlAddFormula (toRC "B1") (num 0)),                                       XlNumber 15 ), -- final state
            ( (XlAddFormula (toRC "A2") (XlFun "+" [cell "A1", cell "B1"])),            XlNumber 30 ), -- final state
            ( (XlAddFormula (toRC "B1") (cell "A1")),                                   XlNumber 15 ),
            ( (XlAddFormula (toRC "C1") (str "B")),                                     XlString "B" ),
            ( (XlAddFormula (toRC "C2") (num 1)),                                       XlNumber 1 ),
            ( (XlAddFormula (toRC "B2") (XlFun "INDIRECT" [XlFun "&" [cell "C1", cell "C2"]])), XlNumber 15 ),
            ( (XlAddFormula (toRC "D1") (XlFun "SUM" [range "A1" "B2"]) ),              XlNumber 75 ),
            ( (XlAddFormula (toRC "E1") (XlFun "SUM" [range "B1" "B2"]) ),              XlNumber 30 ),
            ( (XlAddFormula (toRC "F1") (XlFun "SUM" [range "D1" "E1"]) ),              XlNumber 105 ),
            
            ( (XlAddFormula (toRC "H1") (num 10) ),                                     XlNumber 10 ),
            ( (XlAddFormula (toRC "I1") (num (-20)) ),                                  XlNumber (-20) ),
            ( (XlAddFormula (toRC "J1") (num 30) ),                                     XlNumber 30 ),
            ( (XlAddFormula (toRC "I2") (XlFun "ABS" [range "H1" "J1"]) ),              XlNumber 20 ),
            ( (XlAddFormula (toRC "K2") (XlFun "ABS" [range "H1" "J1"]) ),              XlError "#VALUE!" ),
            
            ( (XlAddFormula (toRC "A10") (num 10) ),                                    XlNumber 10 ),
            ( (XlAddFormula (toRC "A11") (str "10") ),                                  XlString "10" ),
            ( (XlAddFormula (toRC "A12") (XlFun "=" [cell "A10", cell "A11"]) ),        XlBoolean False ),
            ( (XlAddFormula (toRC "A13") (XlFun "=" [cell "A10", num 10]) ),            XlBoolean True ),
            ( (XlAddFormula (toRC "A14") (XlFun "=" [cell "A13", num 1]) ),             XlBoolean True ),
   
            ( (XlAddFormula (toRC "A15") (XlFun "/" [num 1, num 0]) ),                  XlError "#DIV/0!" ),
            ( (XlAddFormula (toRC "A16") (XlFun "=" [cell "K2", cell "A15"]) ),         XlError "#VALUE!" ),
            ( (XlAddFormula (toRC "A17") (XlFun "=" [cell "A15", cell "K2"]) ),         XlError "#DIV/0!" ),

            ( (XlAddFormula (toRC "G1") (XlFun "+" [num 1000, range "A1" "A2"]) ),      XlNumber 1015 )
         ]
   
      env@(XlEnv cells values) = run (XlWorksheet Map.empty) (map fst operations)

      value :: String -> XlValue
      value a1 = values ! (toRC a1)

      failures = filter (\v -> v /= Nothing) $ map doCheck operations
         where
            doCheck (op, value) =
               case op of
                  XlAddFormula rc fml -> if values ! rc == value then Nothing else Just rc
                  _ -> Nothing
   in
      do
         print env
         if null failures
         then putStrLn "All passed!"
         else print failures
