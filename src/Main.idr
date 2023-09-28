module Main

import MathML
import MathMLExamples
import Expr

import End2end

-- Function to check the expected output of the pretty printer


public export
mathTestCases2 : List String
mathTestCases2 =
  [ 
    "+",
    "5 + 2",
    "5 * 2",
    "5 - (3 + 2)",
    "(5 + 2) * (3 - 1)",
    "x * (y + z)",
    " + y * z",
    "dfsdfdhj error"
  ]

-- Main function with test cases
main : IO ()
main = do
  
 
  putStrLn (tryComplete "summation(2,4,i*2)" ++ tryComplete("2*2+3*2+4*2"))
  putStrLn (tryComplete "2^4")
  putStrLn (tryComplete  "x+y*z")
  putStrLn (tryComplete "5+2")
  putStrLn (tryComplete "x*(y+z)")
  putStrLn (tryComplete "(4+2)/(7-3)")
  putStrLn (tryComplete "5+3/8-4")
  putStrLn (tryComplete "(2+3)*3")
  putStrLn (tryComplete "[3*4,(2/3)^5,68]")
  
  

  