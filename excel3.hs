
import Data.Map.Strict as Map (Map, empty, elems, mapWithKey, foldrWithKey, member, insert, lookup, toList, (!))
import Data.List (foldl')
import Data.Tree (flatten)
import Data.Char (ord, chr)
import Data.Set as Set (Set, insert, member, empty, union, toList, singleton, fromList)
import Debug.Trace
import Data.Fixed
import Text.PrettyPrint.Boxes as Box (render, hcat, vcat, text)
import Text.PrettyPrint.Boxes as Alignment (left, right)
import ShowConcat ((@@))

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
   show (XlRel n) = "[" @@ n @@ "]"

data XlRC = XlRC XlAddr XlAddr
   deriving (Eq, Ord)

instance Show XlRC where
   show (XlRC r@(XlAbs rn) c@(XlAbs cn)) = "<" @@ [chr (cn + 65)] @@ (rn + 1) @@ ">"
   show (XlRC r c) = "R" @@ r @@ "C" @@ c

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
   show (XlEnv cells values) = 
      "\nCells:\n" @@ listCells @@ "\nValues:\n" @@ tableValues @@ "\n" @@ values @@ "\n"
         where
            maxRow = Map.foldrWithKey (\(XlRC (XlAbs r) _) _ mx -> max r mx) 0 values
            maxCol = Map.foldrWithKey (\(XlRC _ (XlAbs c)) _ mx -> max c mx) 0 values
            listCells   = Box.render $ Box.vcat Alignment.left $ map Box.text $ map show (Map.toList cells)
            tableValues = Box.render $ Box.hcat Alignment.left $ numbersColumn : map doColumn [0..maxCol]
               where 
                  numbersColumn = Box.vcat Alignment.right $ map Box.text $ " " : map show [1..(maxRow + 1)]
                  doColumn c    = Box.vcat Alignment.left  $ Box.text ['|', chr (c + 65)] : map (\s -> Box.text ('|' : doRow s)) [0..maxRow]
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
            
toAbs :: XlRC -> XlRC -> XlRC
toAbs base@(XlRC br bc) cell@(XlRC cr cc) = XlRC (toAbsAddr br cr) (toAbsAddr bc cc)
   where 
      toAbsAddr :: XlAddr -> XlAddr -> XlAddr
      toAbsAddr _ a@(XlAbs _) = a
      toAbsAddr (XlAbs aa) (XlRel rr) = XlAbs (aa + rr)
      toAbsAddr base@(XlRel _) _ = error ("base in toAbs must be absolute, got " @@ base) 

-- Converts Excel addresses in "A1" format to internal RC format.
-- Supports only rows A-Z, and absolute addresses.
toRC :: String -> XlRC
toRC (l:num) = XlRC (XlAbs ((read num) - 1)) (XlAbs ((ord l) - 65)) 

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
   show ev = "Evaluator\nRC: " @@ rc ev @@ "\nVisiting: " @@ visiting ev @@ "\n"

updateRC ev v vs = (v, Map.insert (rc ev) v vs)

evalFormula :: Evaluator -> XlValues -> XlFormula -> (XlValue, XlValues)

evalFormula ev values (XlRng from to) = 
   let 
      (mtx, valuesm) = foldRange (rc ev) from to ([], values) zeroRow cellOp rowOp
         where
            zeroRow :: ([[XlValue]], XlValues) -> ([XlValue], XlValues)
            zeroRow (_, curValues) = ([], curValues)
   
            cellOp :: ([XlValue], XlValues) -> XlRC -> ([XlValue], XlValues)
            cellOp (curRow, curRowValues) curRc =
               addToRow $ (scalar ev) ev curRowValues (XlRef curRc)
                  where addToRow (newValue, newRowValues) = (curRow ++ [newValue], newRowValues)
   
            rowOp :: ([[XlValue]], XlValues) -> Int -> ([XlValue], XlValues) -> ([[XlValue]], XlValues)
            rowOp (curMtx, _) r (newRow, newValues) = (curMtx ++ [newRow], newValues)
   in
      updateRC ev (XlMatrix mtx) valuesm

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
      updateRC ev vr valuesr

evalFormula ev values (XlFun "INDIRECT" [addr]) =
   let
      convert s =
         case break (== ':') s of
            (a1, ':':b2) -> (XlRng (toRC a1) (toRC b2)) -- FIXME error checking in toRC
            _            -> (XlRef (toRC s))            -- FIXME error checking in toRC

      (va, valuesa) = checkString $ (scalar ev) ev values addr
      (vr, valuesr) = 
         case va of
            XlString s -> (scalar ev) ev valuesa (convert s)
            _          -> ((XlError "#VALUE!"), valuesa)
   in
      updateRC ev vr valuesr

evalFormula ev values (XlFun "MID" [vstr, vstart, vlen]) =
   let
      (vstr',   values1) = checkString $ (scalar ev) ev values  vstr
      (vstart', values2) = checkNumber $ (scalar ev) ev values1 vstart
      (vlen',   values3) = checkNumber $ (scalar ev) ev values2 vlen
      
      doMid (XlString str) (XlNumber start) (XlNumber len) = XlString $ take (floor len) $ drop (floor start - 1) str
      doMid a b c = XlError "#VALUE!"
      
      val = doMid vstr' vstart' vlen'
   in
      updateRC ev val values3

evalFormula ev values (XlFun "ABS" [v]) =
   let
      (v', values') = checkNumber $ (scalar ev) ev values v
      
      doAbs (XlNumber n)  = XlNumber $ abs n
      doAbs _             = XlError "#VALUE!"
      
      val = doAbs v'
   in
      updateRC ev val values'

evalFormula ev values (XlFun "SUM" inputs) =
   let
      
      doSum e@(XlError _)  _              = e
      doSum _              e@(XlError _)  = e
      doSum s@(XlString _) v              = v
      doSum v              s@(XlString _) = v
      doSum (XlBoolean b)  (XlNumber n)   = XlNumber (bool2num b + n)
      doSum (XlNumber n)   (XlBoolean b)  = XlNumber (bool2num b + n)
      doSum (XlNumber a)   (XlNumber b)   = XlNumber (a + b)
      
      (vr, valuesr) = foldl' handle (XlNumber 0, values) inputs
         where
            handle (acc, valuesacc) input =
               let
                  (vi, valuesi) = (array ev) ev valuesacc input
                  vsum =
                     case vi of
                        XlMatrix mtx -> foldl' (foldl' doSum) acc mtx
                        XlBoolean b  -> doSum acc vi
                        XlNumber n   -> doSum acc vi
                        _            -> XlError "#VALUE!"
               in
                  (vsum, valuesi)
   in
      updateRC ev vr valuesr

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
      updateRC ev val values''

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
      updateRC ev val values''

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
      updateRC ev val values''

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
      updateRC ev val values''

evalFormula ev va fo = trace ("[ev] " @@ ev @@ "\n[va] " @@ va @@ "\n[fo] " @@ fo @@ "\n") undefined

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
                  Just cell -> give $ fst $ calcCell (Set.insert ref (visiting ev)) cells values ref cell
                  Nothing -> give (XlNumber 0)

calcCell :: Set XlRC -> XlCells -> XlValues -> XlRC -> XlCell -> (XlValue, XlValues)
calcCell visiting cells values myRC@(XlRC (XlAbs r) (XlAbs c)) (XlCell formula) = (scalar ev) ev values formula
   where
   
      ev = Evaluator {
         rc = myRC,
         visiting = visiting,
         scalar = scalarEvaluator,
         array = arrayEvaluator
      }
      
      arrayEvaluator ev values formula =
         case formula of
            XlLit v -> updateRC ev v values
            XlRef ref -> getRef ev cells values ref
            _ -> evalFormula ev values formula
         
      -- NOTE that we can't just evaluate a range to a matrix and convert to scalar,
      -- because explicit ranges are converted via intersection.
      scalarEvaluator ev values formula =
         let
            formula' = toScalar formula
         in
            case formula' of
               XlLit v -> updateRC ev v values
               XlRef ref -> getRef ev cells values ref
               _ -> evalFormula ev values formula'

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

calcCell visiting cells values myRC (XlAFCell formula (x, y)) = (scalar ev) ev values formula
   where
   
      ev = Evaluator {
         rc = myRC,
         visiting = visiting,
         scalar = scalarEvaluator,
         array = arrayEvaluator
      }
      
      arrayEvaluator ev values formula =
         case formula of
            XlLit v -> updateRC ev v values
            XlRef ref -> getRef ev cells values ref
            _ -> evalFormula ev values formula

      scalarEvaluator ev values formula =
         let
            formula' = toScalar formula
         in
            case formula' of
               XlLit v -> updateRC ev v values
               XlRef ref -> getRef ev cells values ref
               _ -> evalFormula ev values formula'

      -- 2) Matrix evaluation
      -- If an expression is being evaluated in a cell flagged as a being part of a 'Matrix'
      -- (OpenDocument 8.1.3 table:number-matrix-columns-spanned):

      -- 2.1) The portion of a non-scalar result to be displayed need not be co-extensive with a
      -- specified display area. The portion of the non-scalar result to be displayed is
      -- determined by:
      displayRule :: Int -> Int -> (Int -> Int -> XlFormula) -> XlFormula
      displayRule sizeX sizeY getXY =
         -- 2.1.1) If the position to be displayed exists in the result, display that position.
         if sizeY > y && sizeX > x
         then getXY x y
         -- [Rules 2.1.2 and 2.1.3 apply to singletons]
         else if sizeX == 1 && sizeY == 1
         then getXY 0 0
         -- 2.1.2) If the non-scalar result is 1 column wide, subsequent columns in the display
         -- area display the value in the first column.
         else if sizeX == 1 && sizeY > y
         then getXY 0 y
         -- 2.1.3) If the non-scalar result is 1 row high, subsequent rows in the display area use
         -- the value of the first row.
         else if sizeY == 1 && sizeX > x
         then getXY x 0
         -- 2.1.4) If none of the other rules apply #N/A
         else XlLit $ XlError "#N/A!"

      toScalar (XlLit (XlMatrix mtx)) =
         displayRule (foldr max 0 (map length mtx)) (length mtx) (\x y -> XlLit $ mtx !! y !! x)

      toScalar (XlRng rcFrom rcTo) =
         let
            XlRC (XlAbs fromR) (XlAbs fromC) = toAbs myRC rcFrom
            XlRC (XlAbs toR)   (XlAbs toC)   = toAbs myRC rcTo
         in
            displayRule (1 + toC - fromC) (1 + toR - fromR) (\x y -> XlRef (XlRC (XlAbs (fromR + y)) (XlAbs (fromC + x))))

      toScalar v = v

updateCells cells event@(XlAddFormula rc formula) =
   Map.insert rc (XlCell formula) cells

updateCells cells event@(XlAddArrayFormula rcFrom rcTo formula) =
   fst $ foldRange rcFrom rcFrom rcTo (cells, (0, 0)) id cellOp rowOp
      where
         cellOp (cells, (x, y)) rc = (Map.insert rc (XlAFCell formula (x, y)) cells, (x + 1, y))
         rowOp _ r (cells, (x, y)) = (cells, (0, y + 1))

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
               else snd $ calcCell (Set.singleton rc) newCells values rc cell
         in
            XlEnv newCells (Map.foldrWithKey acc Map.empty newCells)

   in foldl' runEvent (XlEnv cells Map.empty) events

str :: String -> XlFormula
str s = XlLit (XlString s)

num :: Double -> XlFormula
num n = XlLit (XlNumber n)

nummtx mx = XlLit (XlMatrix (map (map XlNumber) mx))

ref :: String -> XlFormula
ref a1 = XlRef (toRC a1)

range :: String -> String -> XlFormula
range a1 b2 = XlRng (toRC a1) (toRC b2)

main :: IO ()
main = 
   let
      runTest operations =
         let
            env@(XlEnv cells values) = run (XlWorksheet Map.empty) (map fst operations)
            value :: String -> XlValue
            value a1 = values ! (toRC a1)
         
            failures = filter (\v -> v /= Nothing) $ map doCheck operations
               where
                  doCheck (op, value) =
                     case op of
                        XlAddFormula rc fml -> if values ! rc == value then Nothing else Just rc
                        XlAddArrayFormula rcFrom rcTo fml -> if values ! rcFrom == value then Nothing else Just rcFrom
         in do
            print env
            if null failures
            then putStrLn "OK! :-D"
            else
               do
                  putStrLn "****************** Failed: ******************"
                  print failures
   in
      do
         runTest [
            ( XlAddFormula (toRC "A1") (num 15),                                      XlNumber 15 ),
            ( XlAddFormula (toRC "B1") (num 0),                                       XlNumber 15 ), -- final state
            ( XlAddFormula (toRC "A2") (XlFun "+" [ref "A1", ref "B1"]),              XlNumber 30 ), -- final state
            ( XlAddFormula (toRC "B1") (ref "A1"),                                    XlNumber 15 ),
            ( XlAddFormula (toRC "C1") (str "B"),                                     XlString "B" ),
            ( XlAddFormula (toRC "C2") (num 1),                                       XlNumber 1 ),
            ( XlAddFormula (toRC "B2") (XlFun "INDIRECT" [XlFun "&" [ref "C1", ref "C2"]]), XlNumber 15 ),
            ( XlAddFormula (toRC "D1") (XlFun "SUM" [range "A1" "B2"]),              XlNumber 75 ),
            ( XlAddFormula (toRC "E1") (XlFun "SUM" [range "B1" "B2"]),              XlNumber 30 ),
            ( XlAddFormula (toRC "F1") (XlFun "SUM" [range "D1" "E1"]),              XlNumber 105 ),
            
            ( XlAddFormula (toRC "H1") (num 10),                                     XlNumber 10 ),
            ( XlAddFormula (toRC "I1") (num (-20)),                                  XlNumber (-20) ),
            ( XlAddFormula (toRC "J1") (num 30),                                     XlNumber 30 ),
            ( XlAddFormula (toRC "I2") (XlFun "ABS" [range "H1" "J1"]),              XlNumber 20 ),
            ( XlAddFormula (toRC "K2") (XlFun "ABS" [range "H1" "J1"]),              XlError "#VALUE!" ),
            
            ( XlAddFormula (toRC "A10") (num 10),                                    XlNumber 10 ),
            ( XlAddFormula (toRC "A11") (str "10"),                                  XlString "10" ),
            ( XlAddFormula (toRC "A12") (XlFun "=" [ref "A10", ref "A11"]),          XlBoolean False ),
            ( XlAddFormula (toRC "A13") (XlFun "=" [ref "A10", num 10]),             XlBoolean True ),
            ( XlAddFormula (toRC "A14") (XlFun "=" [ref "A13", num 1]),              XlBoolean True ),
   
            ( XlAddFormula (toRC "A15") (XlFun "/" [num 1, num 0]),                  XlError "#DIV/0!" ),
            ( XlAddFormula (toRC "A16") (XlFun "=" [ref "K2", ref "A15"]),           XlError "#VALUE!" ),
            ( XlAddFormula (toRC "A17") (XlFun "=" [ref "A15", ref "K2"]),           XlError "#DIV/0!" ),

            ( XlAddFormula (toRC "G1") (XlFun "+" [num 1000, range "A1" "A2"]),      XlNumber 1015 ),

            ( XlAddFormula (toRC "C5") (range "A1" "A2"),                            XlError "#VALUE!" ),
            ( XlAddArrayFormula (toRC "F5") (toRC "F6") (nummtx [[15], [16]]), XlNumber 15 ),
            ( XlAddArrayFormula (toRC "D5") (toRC "D6") (XlFun "+" [range "A1" "A2", num 100]), XlNumber 115 )
            ]
         -- 3.3 1 1.1) Note 1
         runTest [
            ( XlAddFormula (toRC "A1") (XlFun "ABS" [nummtx [[-3, -4]]]), XlNumber 3 ),
            ( XlAddFormula (toRC "A2") (XlFun "ABS" [nummtx [[-3], [-4]]]), XlNumber 3 ),
            ( XlAddFormula (toRC "A3") (XlFun "ABS" [nummtx [[-3, -4], [-6, -8]]]), XlNumber 3 ),
            ( XlAddFormula (toRC "A4") (nummtx [[1, 2, 3], [4, 5, 6]]), XlNumber 1 )
            ]
         -- 3.3 1 1.2 1.2.1) Notes 2 and 3
         runTest [
            ( XlAddFormula (toRC "A1") (num (-10)), XlNumber (-10) ),
            ( XlAddFormula (toRC "B1") (num (-20)), XlNumber (-20) ),
            ( XlAddFormula (toRC "C1") (num (-30)), XlNumber (-30) ),
            ( XlAddFormula (toRC "B2") (XlFun "ABS" [range "A1" "C1"]), XlNumber 20 ),
            ( XlAddFormula (toRC "D4") (XlFun "ABS" [range "A1" "C1"]), XlError "#VALUE!" )
            ]
         -- 3.3 2 2.1 2.1.4) Note 5.1
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B3") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlAddFormula (toRC "C3") (ref "B2"), XlNumber 4 )
            ]
         -- 3.3 2 2.1 2.1.4) Note 5.2
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B3") (nummtx [[1],[3],[5]]), XlNumber 1 ),
            ( XlAddFormula (toRC "C3") (ref "B2"), XlNumber 3 )
            ]
         -- 3.3 2 2.1 2.1.4) Note 5.3
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B3") (nummtx [[2,4]]), XlNumber 2 ),
            ( XlAddFormula (toRC "C3") (XlRef (toRC "B2")), XlNumber 4 )
            ]
         -- 3.3 2 2.1 2.1.4) Note 5.3
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "C4") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlAddFormula (toRC "D1") (XlRef (toRC "C1")), XlError "#N/A!" ),
            ( XlAddFormula (toRC "D2") (XlRef (toRC "A4")), XlError "#N/A!" )
            ]
         -- 3.3 2 2.1 2.1.4) Note 6
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B2") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlAddFormula (toRC "D1") (XlRef (toRC "B3")), XlNumber 0 )
            ]
         -- 3.3 2 2.2 2.2.1) Note 7
         runTest [
            ( XlAddFormula (toRC "A1") (num 10), XlNumber 10 ),
            ( XlAddFormula (toRC "A2") (num 20), XlNumber 20 ),
            ( XlAddArrayFormula (toRC "A3") (toRC "A3") (XlFun "SUM" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]), XlNumber 10 )
            ]
         -- 3.3 2 2.2 2.2.1) Note 8.1
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "C1") (XlFun "+" [nummtx [[1,2]], nummtx [[3,4,5]]]), XlNumber 4 ),
            ( XlAddFormula (toRC "A2") (ref "A1"), XlNumber 4 ),
            ( XlAddFormula (toRC "B2") (ref "B1"), XlNumber 6 ),
            ( XlAddFormula (toRC "C2") (ref "C1"), XlError "#N/A!" )
            ]
         -- 3.3 2 2.2 2.2.1) Note 8.2
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B1") (XlFun "+" [nummtx [[1]], nummtx [[1,2]]]), XlNumber 2 ),
            ( XlAddFormula (toRC "A2") (ref "A1"), XlNumber 2 ),
            ( XlAddFormula (toRC "B2") (ref "B1"), XlNumber 3 )
            ]
         -- 3.3 2 2.2 2.2.3 2.2.3.1) Note 9.1
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "C2") (XlFun "+" [num 1, nummtx [[1,2,3],[4,5,6]]]), XlNumber 2 ),
            ( XlAddFormula (toRC "D1") (ref "A1"), XlNumber 2 ),
            ( XlAddFormula (toRC "E1") (ref "B1"), XlNumber 3 ),
            ( XlAddFormula (toRC "F1") (ref "C1"), XlNumber 4 ),
            ( XlAddFormula (toRC "D2") (ref "A2"), XlNumber 5 ),
            ( XlAddFormula (toRC "E2") (ref "B2"), XlNumber 6 ),
            ( XlAddFormula (toRC "F2") (ref "C2"), XlNumber 7 )
            ]
         -- 3.3 2 2.2 2.2.3 2.2.3.1) Note 9.2
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "C2") (XlFun "+" [nummtx [[1]], nummtx [[1,2,3],[4,5,6]]]), XlNumber 2 ),
            ( XlAddFormula (toRC "D1") (ref "A1"), XlNumber 2 ),
            ( XlAddFormula (toRC "E1") (ref "B1"), XlNumber 3 ),
            ( XlAddFormula (toRC "F1") (ref "C1"), XlNumber 4 ),
            ( XlAddFormula (toRC "D2") (ref "A2"), XlNumber 5 ),
            ( XlAddFormula (toRC "E2") (ref "B2"), XlNumber 6 ),
            ( XlAddFormula (toRC "F2") (ref "C2"), XlNumber 7 )
            ]
         -- 3.3 2 2.2 2.2.3 2.2.3.1) Note 10
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1],[2]], nummtx [[10,20],[30,40]]]), XlNumber 11 ),
            ( XlAddFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlAddFormula (toRC "E1") (ref "B1"), XlNumber 21 ),
            ( XlAddFormula (toRC "D2") (ref "A2"), XlNumber 32 ),
            ( XlAddFormula (toRC "E2") (ref "B2"), XlNumber 42 )
            ]
         -- 3.3 2 2.2 2.2.3 2.2.3.1) Note 11
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1,2]], nummtx [[10,20],[30,40]]]), XlNumber 11 ),
            ( XlAddFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlAddFormula (toRC "E1") (ref "B1"), XlNumber 22 ),
            ( XlAddFormula (toRC "D2") (ref "A2"), XlNumber 31 ),
            ( XlAddFormula (toRC "E2") (ref "B2"), XlNumber 42 )
            ]
         -- 3.3 2 2.2 2.2.3 2.2.3.1) Note 12
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1,2]], nummtx [[10],[20]]]), XlNumber 11 ),
            ( XlAddFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlAddFormula (toRC "E1") (ref "B1"), XlNumber 12 ),
            ( XlAddFormula (toRC "D2") (ref "A2"), XlNumber 21 ),
            ( XlAddFormula (toRC "E2") (ref "B2"), XlNumber 22 )
            ]
         -- 3.3 2 2.2 2.2.3 2.2.3.1) Note 13
         runTest [
            ( XlAddArrayFormula (toRC "A1") (toRC "C1") (XlFun "MID" [str "abcd", nummtx [[1,2]], nummtx [[1,2,3]]]), XlString "a" ),
            ( XlAddFormula (toRC "A2") (ref "A1"), XlString "a" ),
            ( XlAddFormula (toRC "B2") (ref "B1"), XlString "bc" ),
            ( XlAddFormula (toRC "C2") (ref "C1"), XlError "#VALUE!" )
            ]
