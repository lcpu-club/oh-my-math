module Expr

--import Data.List1
import public Derive.Prelude
--import Text.Lex
--import Text.Parse
--import Text.Parse.Manual
--import Data.Either

--%default total
%language ElabReflection

public export
data MathExpr : Type where
  Lit3 : Double -> MathExpr
  Add2 : MathExpr -> MathExpr -> MathExpr
  Sub2 : MathExpr -> MathExpr -> MathExpr
  Mul2 : MathExpr -> MathExpr -> MathExpr
  Div2 : MathExpr -> MathExpr -> MathExpr
  Var2 : String -> MathExpr
  Pow2 : MathExpr -> MathExpr -> MathExpr
  Vector2 : List MathExpr -> MathExpr
  Parentheses2 : MathExpr-> MathExpr
  Summation2 : MathExpr -> MathExpr -> MathExpr -> MathExpr

%runElab derive "MathExpr" [Show, Eq]

public export
data MathToken : Type where
  Symbol   : Char -> MathToken
  Lit      : MathExpr -> MathToken
  Space    : MathToken

%runElab derive "MathToken" [Show, Eq]

public export
data JSErr : Type where
  ExpectedString  : JSErr

%runElab derive "JSErr" [Show, Eq]

public export %inline
fromChar : Char -> MathToken
fromChar = Symbol

export
Interpolation MathToken where
  interpolate (Symbol c) = show c
  interpolate (Lit x)    = "'\{show x}'"
  interpolate Space      = "<spaces>"

export
Interpolation JSErr where
  interpolate ExpectedString  = "Expected string literal"

