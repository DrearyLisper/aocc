module AOCC.Grammar where

import AOCC.Parsing
import Control.Applicative

data Function
  = Function
      { functionName :: String,
        functionArguments :: [String]
      }
      deriving (Show)

data Package = Package
  { packageName :: String,
    packageFunctions :: [Function]
  }
  deriving (Show)

function :: Parser Function
function = do
  _ <- symbol "fun"
  functionName <- identifier
  functionArguments <- list "(" ")" "," identifier
  return $
    Function
      { functionName = functionName,
        functionArguments = functionArguments
      }

package :: Parser Package
package = do
  _ <- symbol "package"
  packageName <- identifier
  packageFunctions <- many $ token function
  return $
    Package
      { packageName = packageName,
        packageFunctions = packageFunctions
      }
