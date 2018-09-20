module Lang.Syntax where

import qualified Language.Java.Syntax as Java

type Name = String

data Expr
  = Int Int
  | Byte Word
  | Char Word
  | Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)