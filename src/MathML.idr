module MathML

import Derive.Prelude

import Expr

%default total
%language ElabReflection


public export
data MathML : Type where
  Mrow : List MathML -> MathML
  Mfrac : MathML -> MathML -> MathML
  Msqrt : MathML -> MathML
  Mroot : MathML -> MathML -> MathML -> MathML
  Msub : MathML -> MathML -> MathML
  Msup : MathML -> MathML -> MathML
  Msubsup : MathML -> MathML -> MathML -> MathML
  Munder : MathML -> MathML -> MathML
  Mover : MathML -> MathML -> MathML
  Munderover : MathML -> MathML -> MathML -> MathML
  Mo : String -> MathML
  Mi : String -> MathML
  Mn : String -> MathML
  Mtable : List MathML -> MathML -- https://danielscully.uk/projects/mathml-guide/vectorsmatrices.php
  Mtr : List MathML -> MathML
  Mtd : MathML -> MathML -- should i use refinement types here to declare which child elements are allowed?

%runElab derive "MathML" [Show,Eq]


export
partial
prettyPrintMathML : MathML -> String
prettyPrintMathML (Mrow children) = "<mrow>" ++ concatMap prettyPrintMathML children ++ "</mrow>"
prettyPrintMathML (Mfrac num denom) = "<mfrac>" ++ prettyPrintMathML num ++ prettyPrintMathML denom ++ "</mfrac>"
prettyPrintMathML (Msqrt expr) = "<msqrt>" ++ prettyPrintMathML expr ++ "</msqrt>"
prettyPrintMathML (Mroot base degree expr) = "<mroot>" ++ prettyPrintMathML base ++ prettyPrintMathML degree ++ prettyPrintMathML expr ++ "</mroot>" -- check whether this printing is correct
prettyPrintMathML (Msub base subscript) = "<msub>" ++ prettyPrintMathML base ++ prettyPrintMathML subscript ++ "</msub>"
prettyPrintMathML (Msup base superscript) = "<msup>" ++ prettyPrintMathML base ++ prettyPrintMathML superscript ++ "</msup>"
prettyPrintMathML (Msubsup base subscript superscript) = "<msubsup>" ++ prettyPrintMathML base ++ prettyPrintMathML subscript ++ prettyPrintMathML superscript ++ "</msubsup>"
prettyPrintMathML (Munder base underscript) = "<munder>" ++ prettyPrintMathML base ++ prettyPrintMathML underscript ++ "</munder>"
prettyPrintMathML (Mover base overscript) = "<mover>" ++ prettyPrintMathML base ++ prettyPrintMathML overscript ++ "</mover>"
prettyPrintMathML (Munderover base underscript overscript) = "<munderover>" ++ prettyPrintMathML base ++ prettyPrintMathML underscript ++ prettyPrintMathML overscript ++ "</munderover>"
prettyPrintMathML (Mo op) = "<mo>" ++ op ++ "</mo>"
prettyPrintMathML (Mi ident) = "<mi>" ++ ident ++ "</mi>"
prettyPrintMathML (Mn num) = "<mn>" ++ num ++ "</mn>"
prettyPrintMathML (Mtable row) = "<mtable>" ++ concatMap prettyPrintMathML row ++ "</mtable>"  --TODO
prettyPrintMathML (Mtr column) = "<mtr>" ++ concatMap prettyPrintMathML column ++ "</mtr>"  --TODO
prettyPrintMathML (Mtd elem) = "<mtd>" ++ prettyPrintMathML elem ++ "<mtd/>" 
--prettyPrintMathML _ = "<mtable>" ++ "</mtable>"  --TODO

{- 

 <mrow>
    <mo>(</mo>
    <mtable>
      <mtr>
        <mtd>
          <mi>x</mi>
        </mtd>
      </mtr>
      <mtr>
        <mtd>
          <mi>y</mi>
        </mtd>
      </mtr>
    </mtable>
    <mo>)</mo>
  </mrow>

-}
  


export
partial
mathExprToMathML : MathExpr -> MathML
mathExprToMathML (Lit3 x) = Mn (show x)
mathExprToMathML (Add2 e1 e2) = Mrow [mathExprToMathML e1, Mo "+", mathExprToMathML e2]
mathExprToMathML (Sub2 e1 e2) = Mrow [mathExprToMathML e1, Mo "-", mathExprToMathML e2]
mathExprToMathML (Mul2 e1 e2) = Mrow [mathExprToMathML e1, Mo "*", mathExprToMathML e2]
mathExprToMathML (Div2 e1 e2) = Mfrac (mathExprToMathML e1) (mathExprToMathML e2)
mathExprToMathML (Pow2 e1 e2) = Msup (mathExprToMathML e1) (mathExprToMathML e2)
mathExprToMathML (Var2 v) = Mi v
--mathExprToMathML (Vector2 row) = Mrow (map mathExprToMathML row) -- makes function partial
mathExprToMathML (Vector2 row) = Mrow [Mo "[", Mtable (map (\e => Mtr [mathExprToMathML e]) row), Mo "]"]
--mathExprToMathML (Summation2 index lower upper expression) = Mrow[Munder [Mover [∑], Mrow[Mi[],Mo[],Mn[]],mi[n]],Mi[index],Mo["("],Mi[index]]
mathExprToMathML (Parentheses2 expr) = Mrow[Mo "(", mathExprToMathML expr, Mo ")"]
--mathExprToMathML (Parentheses2 expr) = Mrow[Mo "(", Mo ")"]
mathExprToMathML (Summation2 lower upper expression) =
  Mrow [
    Munderover (Mo "&sum;") (Mi  ( "i=" ++(prettyPrintMathML (mathExprToMathML lower)) ))  (  Mn (prettyPrintMathML (mathExprToMathML upper ))   )
    {- 
    Mo "(",
    Mrow [
      Mo "(",
      Mi (prettyPrintMathML (mathExprToMathML index)),
      Mo "=",
      Mn (prettyPrintMathML (mathExprToMathML lower)),
      Mo ")",
      Mo ",",
      Mn (prettyPrintMathML (mathExprToMathML upper)),
      Mo ")"
    ],
    Mo "(",
    mathExprToMathML expression,
    Mo ")",
    Mo ")"

    -}
    ,Mo (prettyPrintMathML (mathExprToMathML expression))
    ,Mo "="
  ]
-- lower bound https://en.wikipedia.org/wiki/Summation