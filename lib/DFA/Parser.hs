{-# LANGUAGE OverloadedStrings #-}

module DFA.Parser where

import DFA.DFA
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (empty)
import Data.Text hiding (empty, map, concatMap, filter)
import Data.Void
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromJust)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

type Parser = Parsec Void Text

data Node q = Node {
    name :: q,
    isInitial :: Bool,
    isAccepting :: Bool
} deriving (Show)

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where 
    lineCmnt = L.skipLineComment "%"
    blockCmnt = empty 

lexeme :: Parser a -> Parser a 
lexeme = L.lexeme sc

symbol :: Text -> Parser Text 
symbol = L.symbol sc

braces :: Parser a -> Parser a 
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a 
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a 
parens = between (symbol "(") (symbol ")")

semi :: Parser Text 
semi = symbol ";"

dfaParser :: (Ord q, Ord a) => Parser q -> Parser a -> Parser (DFA q a)
dfaParser nameParser symbolParser = do 
    nodes <- some (nodeParser nameParser)
    let init = name . Prelude.head $ filter isInitial nodes
        accept = map name $ filter isAccepting nodes
    
        stateNames = S.fromList $ map name nodes 
    paths <- pathParser nameParser symbolParser
    let transitions = concatMap (\(startNode, pathData) ->
                        concatMap (\(symbols, endNode) ->
                            map (\symbol ->
                                ((startNode, symbol), endNode)) symbols) pathData) paths
        alphabet = S.fromList . map (snd . fst) $ transitions

    return $ DFA stateNames 
                 alphabet 
                 (M.fromList transitions) 
                 init 
                 (S.fromList accept)

nodeParser :: Parser q -> Parser (Node q)
nodeParser nameParser = lexeme $ do
    symbol "\\node"
    flags <- brackets (sepBy1 (some alphaNumChar) (symbol ","))

    let initial = "initial" `elem` flags 
        accepting = "accepting" `elem` flags 

    nodeName <- parens nameParser

    optional nodePositionDescParser
    optional $ braces (char '$' *> some (alphaNumChar <|> char '_') <* char '$')
    semi

    return $ Node nodeName initial accepting

pathParser :: Parser q -> Parser a -> Parser [(q, [([a],q)])]
pathParser nameParser symbolParser = lexeme $ do 
    symbol "\\path"

    paths <- some (singlePathParser nameParser symbolParser)

    semi

    return paths

singlePathParser :: Parser q -> Parser a -> Parser (q, [([a], q)])
singlePathParser nameParser symbolParser = lexeme $ do 
    name <- parens nameParser 
    endpoints <- some (endpointParser nameParser symbolParser)
    return (name, endpoints)

endpointParser :: Parser q -> Parser a -> Parser ([a],q)
endpointParser nameParser symbolParser = lexeme $ do 
    symbol "edge"
    optional pathPositionDescParser
    symbol "node"

    transition <- braces (sepBy1 symbolParser (symbol ","))
    endpoint <- parens nameParser

    return (transition, endpoint)

pathPositionDescParser :: Parser () 
pathPositionDescParser = brackets $ do 
    pathType <- symbol "loop" <|> symbol "bend"
    pathPos <- symbol "above" <|> symbol "left" <|> symbol "below" <|> symbol "right"
    optional (symbol "above" <|> symbol "left" <|> symbol "below" <|> symbol "right")
    extra <- optional (symbol "=" *> some digitChar)

    return ()

nodePositionDescParser :: Parser ()
nodePositionDescParser = brackets $ do 
    symbol "right" <|> symbol "above" <|> symbol "left" <|> symbol "below"
    optional (symbol "above" <|> symbol "left" <|> symbol "below" <|> symbol "right")
    symbol "of"
    symbol "="
    nodeLabel

    return ()

labelNameParser :: Parser ()
labelNameParser = braces (try $ between (symbol "$") (symbol "$") bareLabelName <|> bareLabelName) *> pure ()
    where bareLabelName = some (alphaNumChar <|> char '_')

nodeLabel :: Parser Text 
nodeLabel = pack <$> some alphaNumChar


parseDFAFile :: (Ord q, Ord a) => Parser q -> Parser a -> FilePath -> IO (Either (ParseError Char Void) (DFA q a))
parseDFAFile n s f = parse (dfaParser n s) <$> pure f <*> TIO.readFile f

defaultParseDFAFile :: FilePath -> IO (Either (ParseError Char Void) (DFA String Char))
defaultParseDFAFile = parseDFAFile (some alphaNumChar) (oneOf ['0', '1'])