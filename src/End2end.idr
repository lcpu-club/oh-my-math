module End2end

import public Parser
import MathML
import Expr

import public Data.List1--
import public Text.Parse -- encapsulation might be breaking down 

public export
convertMathExprToMathMLString : String -> Either (List1 (FileContext, JSParseErr)) String
--convertMathExprToMathMLString formula = (parse2 formula) <*> pure ( mathExprToMathML second) 
convertMathExprToMathMLString formula = prettyPrintMathML <$> mathExprToMathML <$> parse2 formula 

public export
tryComplete : String -> String
tryComplete s = either (printParseErrors s) show (convertMathExprToMathMLString s)

public export
mathTestCases : List String
mathTestCases =
  [ 
    "+",
    "5 + 2",
    "5 * 2",
    "5 - (3 + 2)",
    "(5 + 2) * (3 - 1)",
    "x * (y + z)",
    "x + y * z",
    "dfsdfdhj error"
  ]

public export
runTests : Either (List1 (FileContext, JSParseErr)) String
runTests =
  let num = 2 + 3
  in convertMathExprToMathMLString "3"
