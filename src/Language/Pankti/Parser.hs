module Language.Pankti.Parser (
	parsePkt
	) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

import Control.Applicative hiding (many, (<|>))

import Language.Pankti.AST

-- | Parse top-level Statement
parseStatement :: Parser Statement
parseStatement = parseFuncDecl

-- FUNC related functions -----------------------

-- | Parse FuncDecl
parseFuncDecl :: Parser Statement
parseFuncDecl = do string "function" <* space
                   returnType <- parseReturnType
                   space
                   name <- many1 letter
                   char '('
                   params <- parseParameter `sepBy` (char ',')
                   char ')'
                   return $ FuncDecl returnType name params

-- | Parse ReturnType
parseReturnType :: Parser DataType
parseReturnType = do keyword <- many1 letter
                     return $ case keyword of
                          "void" -> Void
                          "int" -> Integer
                          "float" -> Float
                          "double" -> Double
                          "char" -> Char
                          "string" -> String
                          "bool" -> Bool

-- | Parse DataType
parseDataType :: Parser DataType
parseDataType = do keyword <- many1 letter
                   return $ case keyword of
                          "int" -> Integer
                          "float" -> Float
                          "double" -> Double
                          "char" -> Char
                          "string" -> String
                          "bool" -> Bool

-- | Parse Parameter
parseParameter :: Parser Parameter
parseParameter = do dataType <- parseDataType
                    space
                    name <- many1 letter
                    return $ (dataType, name)

-- END FUNC related functions -------------------

parsePkt :: FilePath -> IO (Either ParseError Program)
parsePkt = parseFromFile (parseStatement `sepEndBy` newline)
