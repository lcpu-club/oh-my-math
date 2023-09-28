module Parser

import public Expr

import Data.List1
--import Derive.Prelude
import Text.Lex
import Text.Parse
import Text.Parse.Manual
import Data.Either

%language ElabReflection


public export %tcinline
0 JSParseErr : Type
JSParseErr = ParseError MathToken JSErr

--%runElab derive "JSParseErr" [Show,Eq]


numberLit : Lexer
numberLit
  = let sign  = is '-'
        whole = is '0' <|> range '1' '9' <+> many digit
        frac  = is '.' <+> digits
        exp   = like 'e' <+> opt (oneOf ['+','-']) <+> digits in
        opt sign <+> whole <+> opt frac <+> opt exp

jsstring : Lexer
jsstring = jsonChar
  where
    jsonChar : Lexer
    jsonChar =
          (is '\\' <+> oneOf ['\\','"','n','f','b','r','t','/'])
      <|> (exact "\\u" <+> exactly 4 (pred isHexDigit))
      <|> non (pred isControl <|> is '"')

sumKeyword : Lexer
sumKeyword = exact "summation("


--Compared to these two lexers, the rest is very simple. All we have to do is to collect the lexers in a TokenMap, where lexers are paired with functions for converting the corresponding lexemes to values of type MathToken:

public export
jsonTokenMap : TokenMap MathToken
jsonTokenMap =
  [ (spaces, const Space)
  , (is '+', const (Symbol '+'))
  , (is '-', const (Symbol '-'))
  , (is '*', const (Symbol '*'))
  , (is '/', const (Symbol '/'))
  , (is '(', const (Symbol '('))
  , (is ')', const (Symbol ')'))
  , (is '^', const (Symbol '^'))
  , (is '[', const (Symbol '['))
  , (is ']', const (Symbol ']'))
  , (is ',', const (Symbol ','))
  , (sumKeyword, const (Symbol '?'))
  , (numberLit, Lit . Lit3 . cast . cast {to = String})
  , (jsstring, Lit . Var2 . cast)
  
  ]

tokJSON2 :
     String
  -> Either (Bounded $ ParseError Void Void) (List $ Bounded MathToken)
tokJSON2 = lexManual (first jsonTokenMap)

-- Grammar for mathematical expressions

0 Rule2 : Bool -> Type -> Type
Rule2 b t = Grammar b () MathToken JSErr t


lit : Rule2 True MathExpr

sub : Rule2 True MathExpr

sum : Rule2 True MathExpr

mul : Rule2 True MathExpr

var : Rule2 True MathExpr

div : Rule2 True MathExpr

summation : Rule2 True MathExpr

paren : Rule2 True MathExpr

covering
array2 : Rule2 True MathExpr


lit = terminal $ \case
  Lit j => Just j
  _     => Nothing


var = Var2 <$> terminal (\case
  Lit (Var2 v) => Just v
  _            => Nothing)


paren = Parentheses2 <$> (is '(' *> sum <* is ')')

array2 = Vector2 <$> between (is '[') (is ']') (sepBy (is ',') sum)

summation = Summation2 <$> (is '?' *> sum <* is ',') <*> (sum <* is ',') <*> (sum <* is ')')

atom =  summation <|> array2 <|> paren <|> lit <|> var <|> is '(' *> sum <* is ')'
--summation = foldl Pow2 <$> between (is '?') (is '?') (sepBy (is ',') atom)

pow = foldl Pow2 <$> atom <*> many (is '^' *> atom)

div = foldl Div2 <$> pow <*> many (is '/' *> pow)

mul = foldl Mul2 <$> div <*> many (is '*' *> div)

sub = foldl Sub2 <$> mul <*> many (is '-' *> mul)

sum = foldl Add2 <$> sub <*> many (is '+' *> sub)


{- 
sub = Sub2 <$> lit <*> (is '-' *> value2)

add2 : Rule2 True MathExpr
add2 = Add2 <$> lit <*> (is '+' *> value2)

sum = add2 <|> value2

mul = Mul2 <$> lit <*> (is '*' *> value2)

div = Div2 <$> lit <*> (is '/' *> value2)

var = Var2 <$> terminal (\case Lit (Var2 v) => Just v; _ => Nothing)
-}

-- Parser entry point
public export
parse2 : String -> Either (List1 (FileContext, JSParseErr)) MathExpr
parse2 s = case tokJSON2 s of
  Left x  => Left (singleton $ fromBounded Virtual $ map fromVoid x)
--Right x => case parse value2 () x of
  Right x => case parse sum () x of
    --Right x => case parse array () x of 
    Left es                => Left (fromBounded Virtual <$> es)
    Right ((), res, [])    => Right res
    Right ((), _, (x::xs)) => Left (singleton $ fromBounded Virtual $ Unexpected . Right <$> x)


--public export
--testParse2 : String -> IO ()
--testParse2 s = putStrLn $ either (printParseErrors s) show (parse2 s)

tryParse2 : String -> String
tryParse2 s = either (printParseErrors s) show (parse2 s)

public export
runTestCases : List String -> IO ()
runTestCases [] = putStrLn "All test cases passed!"
runTestCases (t::ts) = do
  putStrLn ("Running test case: " ++ show t)
  --testParse2 (concatMap show t) -- Convert the list of MathToken to a String
  putStrLn ""
  runTestCases ts


