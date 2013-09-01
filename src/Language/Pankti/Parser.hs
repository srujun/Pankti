module Language.Pankti.Parser (
  parsePktFile
  ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

import Control.Applicative hiding (many, (<|>))

import Language.Pankti.AST

-- | Parse .pkt Pankti file
parsePktFile :: FilePath -> IO (Either ParseError Pankti)
parsePktFile = parseFromFile parsePankti

-- | Parse Pankti to tree
parsePankti :: Parser Pankti
parsePankti = do string "package" <* space
                 package <- many1 $ alphaNum <|> char '.'
                 many1 newline
                 imports <- parseImport `sepEndBy` (many1 newline)
                 classes <- parseClass `sepEndBy1` (many1 newline)
                 return $ Pankti package imports classes

-- | Parse import in Pankti file
parseImport :: Parser String
parseImport = do string "import" <* space
                 package <- many1 (alphaNum <|> char '.')
                 return package

-- Class related functions ----------------------
-- | Parse a class
parseClass :: Parser Class
parseClass = do string "class" <* space
                name <- many1 alphaNum
                many1 newline
                statements <- parseStatement `sepEndBy` (many1 newline)
                return $ Class name statements

-- | Parse top-level Statement
parseStatement :: Parser Statement
parseStatement = do tab
                    parseFuncDecl <|> parseVarDecl

-- Function related functions -------------------
-- | Parse top-level-only FuncDecl
parseFuncDecl :: Parser Statement
parseFuncDecl = do returnType <- parseReturnType
                   space
                   name <- many1 alphaNum
                   space *> string "::" *> space
                   params <- parseParameter `sepBy` (char ',')
                   newline
                   body <- parseFuncDeclBody `sepEndBy1` newline
                   return $ FuncDecl returnType name params body

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

-- | Parse Parameter
parseParameter :: Parser Parameter
parseParameter = do dataType <- parseDataType
                    space
                    name <- many1 alphaNum
                    return $ (dataType, name)

-- | Parse FuncDecl body
parseFuncDeclBody :: Parser Statement
parseFuncDeclBody = do parseVarDecl

-- VAR related functions ------------------------
-- | Parse VarDecl
parseVarDecl :: Parser Statement
parseVarDecl = do tab *> tab
                  varType <- parseDataType
                  space
                  name <- many1 alphaNum
                  value <- try parseVarDeclValue
                  let castVal = case varType of
                                  Integer -> Int (read value :: Integer)
                                  Float -> Flo (read value :: Float)
                                  Double -> Dou (read value :: Double)
                                  Char -> Cha (read value :: Char)
                                  String -> Str value
                                  Bool -> Boo (read value :: Bool)
                  return $ VarDecl varType name castVal

-- | Parse value in VarDecl
parseVarDeclValue :: Parser String
parseVarDeclValue = do space *> char '=' *> space
                       value <- manyTill anyChar (try newline)
                       return value

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
