module Language.Pankti.AST (
  Pankti (..),
  Class (..),
  Statement (..),
  Parameter,
  DataType (..),
  Value (..)
  ) where

data Pankti = Pankti {package :: String, imports :: [String], classes :: [Class]}
              deriving (Show)

data Class = Class {className :: String, statements :: [Statement]}
             deriving (Show)

data Statement = VarDecl {dataType :: DataType, varName :: String, value :: Value}
               | FuncDecl {returnType :: DataType, name :: String, parameterDecls :: [Parameter], body :: [Statement]}
               | FuncCall {name :: String, parameters :: [Value]}
                 deriving (Show)

type Parameter = (DataType, String)

data DataType = Void | Integer | Float | Double | Char | String | Bool
                deriving (Show)

data Value = Empty
           | Int Integer
           | Flo Float
           | Dou Double
           | Cha Char
           | Str String
           | Boo Bool
             deriving (Show)


