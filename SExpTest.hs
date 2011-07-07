module SExpTest (
  t1,
  test0, test1, test2,
  str3, test3
  ) where

import Language.LazyS.ParseResult (ParseResult)
import Language.LazyS.SExpSyntax (SExp)
import Language.LazyS.SExpParser (parseExpr)

--parseE = parseExpr

t1 = parseExpr "(x)"

test0 =  parseExpr "(let ((x 1)) x)"

test1 =  parseExpr "(let ((x 1) (y x)) y)"

test2 =  parseExpr "(let ((x 1) (y . (x))) y)"
--test2 = undefined
 
t1, test0, test1, test2 :: ParseResult SExp

str3 :: String
str3 =  unlines
        ["(let (((tak x y z)",
         "       (if (<= x y)",
         "           y",
         "           (tak (tak (- x 1) y z)",
         "                (tak (- y 1) z x)",
         "                (tak (- z 1) x y)))))",
         "  (tak 10 5 1))"]

test3 :: ParseResult SExp
test3 =  parseExpr str3
