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


{-
    \node[initial,state]   (q0) {$q_0$};
    \node[state]           (q1) [right of=q0] {$q_1$};
    \node[state]           (q5) [below left of=q1] {$q_5$};
    \node[state]           (q2) [right of=q1] {$q_2$};
    \node[state]           (q3) [below of=q2] {$q_3$};
    \node[state,accepting] (q4) [below of=q3] {$q_4$};

    \path (q0) edge                 node {1} (q1)
               edge                 node {0} (q5)
          (q1) edge                 node {1} (q5)
               edge                 node {0} (q3)
          (q2) edge  node {1} (q2)
               edge                 node {0} (q3)
          (q3) edge node {0} (q2)
               edge                 node {1} (q4)
          (q4) edge node {0} (q3)
               edge node {1} (q2);
-}

{-
\\node[initial,state] (q0) {$q_0$};\\node[state] (q1) {$q_1$};\\node[state] (q5) {$q_5$};\\node[state] (q2) {$q_2$};\\node[state] (q3) [below of=q2] {$q_3$}; \\node[state,accepting] (q4) {$q_4$}; \\path (q0) edge node {1} (q1) edge node {0} (q5) (q1) edge node {1} (q5) edge node {0} (q3) (q2) edge  node {1} (q2) edge node {0} (q3) (q3) edge node {0} (q2) edge node {1} (q4) (q4) edge node {0} (q3) edge node {1} (q2);
-}
type Parser = Parsec Void Text

data Node = Node {
    name :: Text,
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

--     \node[state]           (q1) [right of=q0] {$q_1$};

dfaParser :: Parser (DFA Text Text)
dfaParser = do 
    nodes <- some nodeParser :: Parser [Node]
    let init = name . Prelude.head $ 
            (filter isInitial nodes :: [Node])
        accept = map name $ filter isAccepting nodes
    
        stateNames = S.fromList $ map name nodes 
    paths <- pathParser
    let transitions = concatMap (\(startNode, pathData) ->
            concatMap (\(symbols, endNode) ->
                map (\symbol ->
                    ((startNode, symbol), endNode)) symbols) pathData) paths
        alphabet = S.fromList . map (snd . fst) $ transitions

    return $ DFA stateNames alphabet (M.fromList transitions) init (S.fromList accept)

nodeParser :: Parser Node 
nodeParser = lexeme $ do
    symbol "\\node"
    flags <- brackets (sepBy1 (some alphaNumChar) (symbol ","))

    let initial = "initial" `elem` flags 
        accepting = "accepting" `elem` flags 

    nodeName <- pack <$> parens (some alphaNumChar)

    optional nodePositionDescParser
    optional $ braces (char '$' *> some (alphaNumChar <|> char '_') <* char '$')
    semi

    return $ Node nodeName initial accepting

pathParser :: Parser [(Text, [([Text],Text)])]
pathParser = lexeme $ do 
    symbol "\\path"

    paths <- some singlePathParser

    semi

    return paths

singlePathParser :: Parser (Text, [([Text], Text)])
singlePathParser = lexeme $ do 
    name <- pack <$> parens (some alphaNumChar) 
    endpoints <- some endpointParser
    return (name, endpoints)

endpointParser :: Parser ([Text],Text)
endpointParser = lexeme $ do 
    symbol "edge"
    optional pathPositionDescParser
    symbol "node"

    transition <- braces (sepBy1 (pack <$> some alphaNumChar) (symbol ","))
    endpoint <- parens (pack <$> some alphaNumChar)

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


parseDFAFile :: FilePath -> IO (Either (ParseError Char Void) (DFA Text Text))
parseDFAFile f = parse dfaParser <$> pure f <*> TIO.readFile f