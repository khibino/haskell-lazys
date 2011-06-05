
module Syntax (
  Pat(..), Literal(..),
  Lambda(..), Exp(..), Bind(..),
  Module(..), Program(..)) where

--import SExpSyntax (SExp, SExp'(..))
import SExpSyntax (SExp)

type Var = String
type Mod = String

data Pat = PVar { runPVar :: Var }
--         |
         deriving (Eq, Show, Read)

data Literal n = Num n
               | Str String
               | Quote SExp
               deriving (Eq, Show)

data Lambda n = Lambda { params :: [Pat]
                       , paramsLen :: Int
                       , varParam :: Maybe Var
                       , body :: Exp n
                       }
              deriving (Eq, Show)
              

data Exp n = Lit (Literal n)
           | EVar Var
           | FApp (Exp n) [Exp n]
           | VCall (Exp n) -- 可変引数関数の呼び出し
           | Abs (Lambda n)
           | Let [Bind n] (Exp n)
           deriving (Eq, Show)

data Bind n = BPat Pat (Exp n)
            | BFun Var (Lambda n)
            deriving (Eq, Show)

data Module n = Module { name :: Mod
                       , binds :: [Bind n]
                       }
              deriving (Eq, Show)

data Program n = Program { modules :: [Module n] }
               deriving (Eq, Show)
