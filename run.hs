
import Control.Applicative ((<$>))

import Language.LazyS.PrimNum (PNum)
import Language.LazyS.ParseResult (ParseResult)
import qualified Language.LazyS.SExpParser as SExp
import Language.LazyS.Parser (parseModule)
import Language.LazyS.Evaluator (Result)
import qualified Language.LazyS.Evaluator as Eval

run :: String -> ParseResult (Result PNum)
run sexpr = Eval.run [] <$>
            (SExp.parseExprList sexpr
             >>= parseModule)

main :: IO ()
main =  (run <$> getContents) >>= print
