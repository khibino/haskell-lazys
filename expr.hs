

import Control.Applicative ((<$>))

import Language.LazyS.PrimNum (PNum)
import Language.LazyS.ParseResult (ParseResult)
import qualified Language.LazyS.SExpParser as SExp (parseExpr)
import Language.LazyS.Parser (parseExpr)
import Language.LazyS.Evaluator (evalExp, Result)

run :: String -> ParseResult (Result PNum)
run sexpr = evalExp <$>
            (SExp.parseExpr sexpr
             >>= parseExpr)

main :: IO ()
main = (run <$> getContents) >>= print
