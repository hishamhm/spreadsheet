\begin{code}

main :: IO ()
main = 
      do
         runTest "Example" [
            ( XlSetFormula (toRC "A1") (num 15),                                      XlNumber 15 ),
            ( XlSetFormula (toRC "B1") (num 0),                                       XlNumber 15 ),
            ( XlSetFormula (toRC "A2") (XlFun "+" [ref "A1", ref "B1"]),              XlNumber 30 ),
            ( XlSetFormula (toRC "B1") (ref "A1"),                                    XlNumber 15 ),
            ( XlSetFormula (toRC "C1") (str "B"),                                     XlString "B" ),
            ( XlSetFormula (toRC "C2") (num 1),                                       XlNumber 1 ),
            ( XlSetFormula (toRC "B2") (XlFun "INDIRECT" [XlFun "&" [ref "C1", ref "C2"]]), XlNumber 15 ),
            ( XlSetFormula (toRC "D1") (XlFun "SUM" [range "A1" "B2"]),              XlNumber 75 ),
            ( XlSetFormula (toRC "E1") (XlFun "SUM" [range "B1" "B2"]),              XlNumber 30 ),
            ( XlSetFormula (toRC "F1") (XlFun "SUM" [range "D1" "E1"]),              XlNumber 105 ),
            
            ( XlSetFormula (toRC "H1") (num 10),                                     XlNumber 10 ),
            ( XlSetFormula (toRC "I1") (num (-20)),                                  XlNumber (-20) ),
            ( XlSetFormula (toRC "J1") (num 30),                                     XlNumber 30 ),
            ( XlSetFormula (toRC "I2") (XlFun "ABS" [range "H1" "J1"]),              XlNumber 20 ),
            ( XlSetFormula (toRC "K2") (XlFun "ABS" [range "H1" "J1"]),              XlError "#VALUE!" ),
            
            ( XlSetFormula (toRC "A10") (num 10),                                    XlNumber 10 ),
            ( XlSetFormula (toRC "A11") (str "10"),                                  XlString "10" ),
            ( XlSetFormula (toRC "A12") (XlFun "=" [ref "A10", ref "A11"]),          XlBoolean False ),
            ( XlSetFormula (toRC "A13") (XlFun "=" [ref "A10", num 10]),             XlBoolean True ),
            ( XlSetFormula (toRC "A14") (XlFun "=" [ref "A13", num 1]),              XlBoolean True ),
   
            ( XlSetFormula (toRC "A15") (XlFun "/" [num 1, num 0]),                  XlError "#DIV/0!" ),
            ( XlSetFormula (toRC "A16") (XlFun "=" [ref "K2", ref "A15"]),           XlError "#VALUE!" ),
            ( XlSetFormula (toRC "A17") (XlFun "=" [ref "A15", ref "K2"]),           XlError "#DIV/0!" ),

            ( XlSetFormula (toRC "G1") (XlFun "+" [num 1000, range "A1" "A2"]),      XlNumber 1015 ),

            ( XlSetFormula (toRC "C5") (range "A1" "A2"),                            XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "F5") (toRC "F6") (nummtx [[15], [16]]), XlNumber 15 ),
            ( XlSetArrayFormula (toRC "D5") (toRC "D6") (XlFun "+" [range "A1" "A2", num 100]), XlNumber 115 )
            ]
         runTest "3.3 1 1.1) Note 1" [
            ( XlSetFormula (toRC "A1") (XlFun "ABS" [nummtx [[-3, -4]]]), XlNumber 3 ),
            ( XlSetFormula (toRC "A2") (XlFun "ABS" [nummtx [[-3], [-4]]]), XlNumber 3 ),
            ( XlSetFormula (toRC "A3") (XlFun "ABS" [nummtx [[-3, -4], [-6, -8]]]), XlNumber 3 ),
            ( XlSetFormula (toRC "A4") (nummtx [[1, 2, 3], [4, 5, 6]]), XlNumber 1 )
            ]
         runTest "3.3 1 1.2 1.2.1) Notes 2 and 3" [
            ( XlSetFormula (toRC "A1") (num (-10)), XlNumber (-10) ),
            ( XlSetFormula (toRC "B1") (num (-20)), XlNumber (-20) ),
            ( XlSetFormula (toRC "C1") (num (-30)), XlNumber (-30) ),
            ( XlSetFormula (toRC "B2") (XlFun "ABS" [range "A1" "C1"]), XlNumber 20 ),
            ( XlSetFormula (toRC "D4") (XlFun "ABS" [range "A1" "C1"]), XlError "#VALUE!" )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.1" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B3") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlSetFormula (toRC "C3") (ref "B2"), XlNumber 4 )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.2" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B3") (nummtx [[1],[3],[5]]), XlNumber 1 ),
            ( XlSetFormula (toRC "C3") (ref "B2"), XlNumber 3 )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.3" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B3") (nummtx [[2,4]]), XlNumber 2 ),
            ( XlSetFormula (toRC "C3") (XlRef (toRC "B2")), XlNumber 4 )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 5.3" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C4") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlSetFormula (toRC "D1") (XlRef (toRC "C1")), XlError "#N/A" ),
            ( XlSetFormula (toRC "D2") (XlRef (toRC "A4")), XlError "#N/A" )
            ]
         runTest "3.3 2 2.1 2.1.4) Note 6" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (nummtx [[1,2],[3,4],[5,6]]), XlNumber 1 ),
            ( XlSetFormula (toRC "D1") (XlRef (toRC "B3")), XlNumber 0 )
            ]
         runTest "3.3 2 2.2 2.2.1) Note 7 (oasis_note7.ods)" [
            ( XlSetFormula      (toRC "A1")             (num 10), XlNumber 10 ),
            ( XlSetFormula      (toRC "A2")             (num 20), XlNumber 20 ),

            ( XlSetFormula      (toRC "B3")             (XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]), XlNumber 10 ),
            ( XlSetArrayFormula (toRC "B4") (toRC "C4") (XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]), XlNumber 10 ),
            ( XlSetFormula      (toRC "B5")             (XlFun "SUM" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]), XlNumber 10 ),
            ( XlSetArrayFormula (toRC "B6") (toRC "B6") (XlFun "SUM" [XlFun "INDIRECT" [XlLit (XlMatrix [[XlString "A1", XlString "A2"]])]]), XlNumber 30 ),
            ( XlSetFormula      (toRC "B7")             (XlFun "SUM" [XlFun "SQRT" [range "A1" "A2"]]), XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "B8") (toRC "B8") (XlFun "SUM" [XlFun "SQRT" [range "A1" "A2"]]), XlNumber (sqrt 10 + sqrt 20) ),
            ( XlSetFormula      (toRC "C7")             (XlFun "SQRT" [range "A1" "A2"]), XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "C8") (toRC "C9") (XlFun "SQRT" [range "A1" "A2"]), XlNumber (sqrt 10) ),

            ( XlSetFormula      (toRC "D3")             (nummtx [[10, 20]]), XlNumber 10 ),
            ( XlSetArrayFormula (toRC "D4") (toRC "E4") (nummtx [[10, 20]]), XlNumber 10 ),
            ( XlSetFormula      (toRC "D5")             (XlFun "SUM" [nummtx [[10,20]]]), XlNumber 30 ),
            ( XlSetArrayFormula (toRC "D6") (toRC "D6") (XlFun "SUM" [nummtx [[10,20]]]), XlNumber 30 ),
            ( XlSetFormula      (toRC "D7")             (XlFun "SUM" [XlFun "SQRT" [nummtx [[10,20]]]]), XlNumber (sqrt 10) ),
            ( XlSetArrayFormula (toRC "D8") (toRC "D8") (XlFun "SUM" [XlFun "SQRT" [nummtx [[10,20]]]]), XlNumber (sqrt 10 + sqrt 20) ),
            ( XlSetFormula      (toRC "E7")             (XlFun "SQRT" [nummtx [[10,20]]]), XlNumber (sqrt 10) ),
            ( XlSetArrayFormula (toRC "E8") (toRC "E9") (XlFun "SQRT" [nummtx [[10],[20]]]), XlNumber (sqrt 10) )
            ]
         runTest "3.3 2 2.2 2.2.1) Note 8.1" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C1") (XlFun "+" [nummtx [[1,2]], nummtx [[3,4,5]]]), XlNumber 4 ),
            ( XlSetFormula (toRC "A2") (ref "A1"), XlNumber 4 ),
            ( XlSetFormula (toRC "B2") (ref "B1"), XlNumber 6 ),
            ( XlSetFormula (toRC "C2") (ref "C1"), XlError "#N/A" )
            ]
         runTest "3.3 2 2.2 2.2.1) Note 8.2" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B1") (XlFun "+" [nummtx [[1]], nummtx [[1,2]]]), XlNumber 2 ),
            ( XlSetFormula (toRC "A2") (ref "A1"), XlNumber 2 ),
            ( XlSetFormula (toRC "B2") (ref "B1"), XlNumber 3 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 9.1" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C2") (XlFun "+" [num 1, nummtx [[1,2,3],[4,5,6]]]), XlNumber 2 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 2 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 3 ),
            ( XlSetFormula (toRC "F1") (ref "C1"), XlNumber 4 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 5 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 6 ),
            ( XlSetFormula (toRC "F2") (ref "C2"), XlNumber 7 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 9.2" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C2") (XlFun "+" [nummtx [[1]], nummtx [[1,2,3],[4,5,6]]]), XlNumber 2 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 2 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 3 ),
            ( XlSetFormula (toRC "F1") (ref "C1"), XlNumber 4 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 5 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 6 ),
            ( XlSetFormula (toRC "F2") (ref "C2"), XlNumber 7 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 10" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1],[2]], nummtx [[10,20],[30,40]]]), XlNumber 11 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 21 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 32 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 42 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 11" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1,2]], nummtx [[10,20],[30,40]]]), XlNumber 11 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 22 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 31 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 42 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 12" [
            ( XlSetArrayFormula (toRC "A1") (toRC "B2") (XlFun "+" [nummtx [[1,2]], nummtx [[10],[20]]]), XlNumber 11 ),
            ( XlSetFormula (toRC "D1") (ref "A1"), XlNumber 11 ),
            ( XlSetFormula (toRC "E1") (ref "B1"), XlNumber 12 ),
            ( XlSetFormula (toRC "D2") (ref "A2"), XlNumber 21 ),
            ( XlSetFormula (toRC "E2") (ref "B2"), XlNumber 22 )
            ]
         runTest "3.3 2 2.2 2.2.3 2.2.3.1) Note 13" [
            ( XlSetArrayFormula (toRC "A1") (toRC "C1") (XlFun "MID" [str "abcd", nummtx [[1,2]], nummtx [[1,2,3]]]), XlString "a" ),
            ( XlSetFormula (toRC "A2") (ref "A1"), XlString "a" ),
            ( XlSetFormula (toRC "B2") (ref "B1"), XlString "bc" ),
            ( XlSetFormula (toRC "C2") (ref "C1"), XlError "#VALUE!" )
            ]

         runTest "ISO/IEC 29500:1 2012 page 2040 ex.1" [
            ( XlSetFormula (toRC "B2") (num 1), XlNumber 1),
            ( XlSetFormula (toRC "B3") (num 2), XlNumber 2),
            ( XlSetFormula (toRC "B4") (num 3), XlNumber 3),
            ( XlSetFormula (toRC "C2") (num 4), XlNumber 4),
            ( XlSetFormula (toRC "C3") (num 5), XlNumber 5),
            ( XlSetFormula (toRC "C4") (num 6), XlNumber 6),
            ( XlSetArrayFormula (toRC "D2") (toRC "D4") (XlFun "+" [XlFun "*" [range "B2" "B4", range "C2" "C4"], num 10.5]), XlNumber 14.5),
            ( XlSetFormula (toRC "E2") (ref "D2"), XlNumber 14.5 ),
            ( XlSetFormula (toRC "E3") (ref "D3"), XlNumber 20.5 ),
            ( XlSetFormula (toRC "E4") (ref "D4"), XlNumber 28.5 )
            ]

         runTest "ISO/IEC 29500:1 2012 page 2040 exs.2 and 3" [
            ( XlSetFormula (toRC "A1") (XlFun "SQRT" [nummtx [[1,2,3,4]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "B1") (toRC "B1") (XlFun "SQRT" [nummtx [[1,2,3,4]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "C1") (toRC "G1") (XlFun "SQRT" [nummtx [[1,2,3,4]]]), XlNumber 1 ),
            ( XlSetFormula (toRC "C2") (ref "C1"), XlNumber (sqrt 1)),
            ( XlSetFormula (toRC "D2") (ref "D1"), XlNumber (sqrt 2)),
            ( XlSetFormula (toRC "E2") (ref "E1"), XlNumber (sqrt 3)),
            ( XlSetFormula (toRC "F2") (ref "F1"), XlNumber (sqrt 4)),
            ( XlSetFormula (toRC "G2") (ref "G1"), XlError "#N/A")
            ]

         runTest "ISO/IEC 29500:1 2012 page 2040 ex.4 ***** consistent with LibreOffice, inconsistent with ISO doc" [
            ( XlSetFormula      (toRC "A1")             (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "A2") (toRC "A2") (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber ((sqrt 1)+(sqrt 2)+(sqrt 3)+(sqrt 4)) )
            ]
         runTest "more tests" [
            ( XlSetFormula      (toRC "A1")             (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber 1 ),
            ( XlSetArrayFormula (toRC "A2") (toRC "A2") (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber ((sqrt 1)+(sqrt 2)+(sqrt 3)+(sqrt 4)) ),
            ( XlSetArrayFormula (toRC "B1") (toRC "E1") (XlFun "SUM" [XlFun "SQRT" [nummtx [[1,2,3,4]]]]), XlNumber ((sqrt 1)+(sqrt 2)+(sqrt 3)+(sqrt 4)) ),
            ( XlSetArrayFormula (toRC "A3") (toRC "D3") (XlFun "SUM" [XlFun "+" [nummtx [[1,2,3,4]], num 100]]), XlNumber 410 )
            ]

         runTest "more tests" [
            ( XlSetFormula      (toRC "A1")             (num 1), XlNumber 1 ),
            ( XlSetFormula      (toRC "A2")             (num 2), XlNumber 2 ),
            ( XlSetFormula      (toRC "A3")             (num 3), XlNumber 3 ),
            ( XlSetFormula      (toRC "A4")             (num 4), XlNumber 4 ),
            ( XlSetFormula      (toRC "B5")             (XlFun "SUM" [XlFun "+" [range "A1" "A4", num 100]]), XlError "#VALUE!" ),
            ( XlSetArrayFormula (toRC "B6") (toRC "B6") (XlFun "SUM" [XlFun "+" [range "A1" "A4", num 100]]), XlNumber 410 )
            ]
         
         runTest "more tests" [
            ( XlSetFormula      (toRC "A3") (str "F"), XlString "F" ),
            ( XlSetFormula      (toRC "B3")             (XlFun "SUM" [XlFun "+" [nummtx [[1,2,3,4]], num 100]]), XlNumber 410 ),
            ( XlSetFormula      (toRC "A4") (str "AF"), XlString "AF" ),
            ( XlSetArrayFormula (toRC "B4") (toRC "B4") (XlFun "SUM" [XlFun "+" [nummtx [[1,2,3,4]], num 100]]), XlNumber 410 ),
            ( XlSetFormula      (toRC "A5") (str "F"), XlString "F" ),
            ( XlSetFormula      (toRC "B5")             (XlFun "SUM" [XlFun "+" [XlFun "ABS" [nummtx [[1,2,3,4]] ], num 100]]), XlNumber 101 ),
            ( XlSetFormula      (toRC "A6") (str "AF"), XlString "AF" ),
            ( XlSetArrayFormula (toRC "B6") (toRC "B6") (XlFun "SUM" [XlFun "+" [XlFun "ABS" [nummtx [[1,2,3,4]] ], num 100]]), XlNumber 410 )
            ]

\end{code}

