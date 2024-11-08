module AOCC.Grammar where

import AOCC.Parsing
import Control.Applicative

data Statement
  = Assignment
      { assignmentLeftSide :: String,
        assignmentRightSide :: String
      }
  | For
      { forVariable :: String,
        forIterable :: String,
        forBody :: Block
      }
  | If
      { ifCondition :: String,
        ifThenBlock :: Block,
        ifElseBlock :: Maybe Block
      }
  | Wildcard
      { wildcardString :: String
      }
  deriving (Show)

data Block
  = Block
  { blockStatements :: [Statement]
  }
  deriving (Show)

data Function
  = Function
  { functionName :: String,
    functionArguments :: [String],
    functionBody :: Block
  }
  deriving (Show)

data Package = Package
  { packageName :: String,
    packageFunctions :: [Function]
  }
  deriving (Show)

statement :: Parser Statement
statement =
  ( do
      _ <- symbol "for"
      forVariable <- identifier
      _ <- symbol "in"
      forIterable <- identifier
      forBlock <- block
      return $
        For
          { forVariable = forVariable,
            forIterable = forIterable,
            forBody = forBlock
          }
  )
    <|> ( do
            _ <- symbol "if"
            ifCondition <- many $ sat (/= '{')
            ifThenBlock <- block
            _ <- symbol "else"
            ifElseBlock <- block
            return
              If
                { ifCondition = ifCondition,
                  ifThenBlock = ifThenBlock,
                  ifElseBlock = Just ifElseBlock
                }
        )
    <|> ( do
            _ <- symbol "if"
            ifCondition <- many $ sat (/= '{')
            ifThenBlock <- block
            return
              If
                { ifCondition = ifCondition,
                  ifThenBlock = ifThenBlock,
                  ifElseBlock = Nothing
                }
        )
    <|> ( do
            wildcardString <- many $ sat (`notElem` [';', '}'])
            _ <- symbol ";"
            return Wildcard {wildcardString = wildcardString}
        )

block :: Parser Block
block = do
  _ <- symbol "{"
  blockStatements <- many statement
  _ <- symbol "}"
  return $
    Block
      { blockStatements = blockStatements
      }

function :: Parser Function
function = do
  _ <- symbol "fun"
  functionName <- identifier
  functionArguments <- list "(" ")" "," identifier
  functionBody <- block
  return $
    Function
      { functionName = functionName,
        functionArguments = functionArguments,
        functionBody = functionBody
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
