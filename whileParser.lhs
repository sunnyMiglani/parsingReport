> module ParserWhile where

>  import System.IO
>  import Control.Monad
>  import Text.ParserCombinators.Parser
>  import Text.ParserCombinators.Parsec.Expr
>  import Text.ParserCombinators.Parsec.Language
>  import qualified Test.ParserCombinators.Parsec.Token as Token

Grammar of the Language

a ::= x | n | -a | a opa a
b ::= true | false | not b | b opb b | a opr a
opa ::= + | - | * | /
opb ::=  and | or
opr ::= > | <

We say a Statement is :

S :: = x := a | skip | S1;S2 | (S) | if b then S1 else S2 | while b do S

We represent this data in haskell's data

> data BExpr = BoolConst Bool | Not BExpr | BBinary BBinOp BExpr BExpr deriving (Show)

> data BBinOp = And | Or deriving (Show)

> data RBinOp = Greater | Less deriving (Show)

> data AExpr = Var String | IntConst Integer | Neg AExpr | ABinary ABinOp AExpr AExpr  deriving (Show)

> data ABinOp = Add | Subtract | Multiply | Divide deriving (Show)

> data Stmt = Seq [Stmt] | Assign String AExpr | If BExpr Stmt Stmt | While BExpr Stmt | Skip deriving (Show)

 We use Haskell's emptyDef from Class "Text.ParserCombinators.Parsec.Language" to create a language defintion

> languageDef =
>   emptyDef { Token.commentStart    = "/*"
>            , Token.commentEnd      = "*/"
>            , Token.commentLine     = "//"
>            , Token.identStart      = letter
>            , Token.identLetter     = alphaNum
>            , Token.reservedNames   = [ "if"
>                                      , "then"
>                                      , "else"
>                                      , "while"
>                                      , "do"
>                                      , "skip"
>                                      , "true"
>                                      , "false"
>                                      , "not"
>                                      , "and"
>                                      , "or"
>                                      ]
>            , Token.reservedOpNames = ["+", "-", "*", "/", ":="
>                                      , "<", ">", "and", "or", "not"
>                                      ]
>            }


> lexer = Token.makeTokenParser languageDef


> identifier = Token.identifier lexer -- parses an identifier
> reserved   = Token.reserved   lexer -- parses a reserved name
> reservedOp = Token.reservedOp lexer -- parses an operator
> parens     = Token.parens     lexer -- parses surrounding parenthesis:
>                                     --   parens p
>                                     -- takes care of the parenthesis and
>                                     -- uses p to parse what's inside them
> integer    = Token.integer    lexer -- parses an integer
> semi       = Token.semi       lexer -- parses a semicolon
> whiteSpace = Token.whiteSpace lexer -- parses whitespace

> whileParser :: Parser Stmt
> whileParser = whiteSpace >> statement

> statement :: Parser Stmt
> statement =   parens statement
>           <|> sequenceOfStmt

> sequenceOfStmt =
>   do list <- (sepBy1 statement' semi)
>      -- If there's only one statement return it without using Seq.
>      return $ if length list == 1 then head list else Seq list

> statement' :: Parser Stmt
> statement' =   ifStmt
>            <|> whileStmt
>            <|> skipStmt
>            <|> assignStmt

> ifStmt :: Parser Stmt
> ifStmt =
>   do reserved "if"
>      cond  <- bExpression
>      reserved "then"
>      stmt1 <- statement
>      reserved "else"
>      stmt2 <- statement
>      return $ If cond stmt1 stmt2

> whileStmt :: Parser Stmt
> whileStmt =
>   do reserved "while"
>      cond <- bExpression
>      reserved "do"
>      stmt <- statement
>      return $ While cond stmt

> assignStmt :: Parser Stmt
> assignStmt =
>   do var  <- identifier
>      reservedOp ":="
>      expr <- aExpression
>      return $ Assign var expr

> skipStmt :: Parser Stmt
> skipStmt = reserved "skip" >> return Skip


> aExpression :: Parser AExpr
> aExpression = buildExpressionParser aOperators aTerm

> bExpression :: Parser BExpr
> bExpression = buildExpressionParser bOperators bTerm

> aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
>              , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
>                 Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
>              , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
>                 Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
>               ]

> bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
>              , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
>                 Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
>              ]


> aTerm =  parens aExpression
>      <|> liftM Var identifier
>      <|> liftM IntConst integer


> bTerm =  parens bExpression
>      <|> (reserved "true"  >> return (BoolConst True ))
>      <|> (reserved "false" >> return (BoolConst False))
>      <|> rExpression


> rExpression =
>   do a1 <- aExpression
>      op <- relation
>      a2 <- aExpression
>      return $ RBinary op a1 a2

> relation =   (reservedOp ">" >> return Greater)
>          <|> (reservedOp "<" >> return Less)


> parseString :: String -> Stmt
> parseString str =
>   case parse whileParser "" str of
>     Left e  -> error $ show e
>     Right r -> r

> parseFile :: String -> IO Stmt
> parseFile file =
>   do program  <- readFile file
>      case parse whileParser "" program of
>        Left e  -> print e >> fail "parse error"
>        Right r -> return r
