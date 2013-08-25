module Language.Pankti.AST (
  Program,
  Statement (..),
  Parameter,
  DataType (..),
  Value (..)
  ) where

type Program = [Statement]

data Statement = VarDecl {dataType :: DataType, name :: String, value :: String}
               | FuncDecl {returnType :: DataType, name :: String,
                           parameters :: [Parameter], body :: [Statement]}
               deriving (Show)

type Parameter = (DataType, String)

data DataType = Void | Integer | Float | Double | Char | String | Bool
                deriving (Show)

data Value = Int Integer
           | Flo Float
           | Dou Double
           | Cha Char
           | Str String
           | Boo Bool
           deriving (Show)


