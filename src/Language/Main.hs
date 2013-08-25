import System.Environment

import Language.Pankti.Parser
import Language.Pankti.AST

main = do
  [path] <- getArgs
  putStrLn $ "Compiling " ++ path
  ast <- parsePkt path
  putStrLn $ show ast
