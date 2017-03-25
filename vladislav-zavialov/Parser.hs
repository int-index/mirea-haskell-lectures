module Parser where

import Control.Applicative
import Control.Monad
import Data.List

data JSON =
  Number Integer |
  List [JSON] |
  Object [(String, JSON)] |
  Str String
  deriving (Eq, Show)

parseJSON ::
  String ->
  Either Err JSON
parseJSON s = fmap fst
  (runParser s pJSON)

pj :: String -> IO ()
pj s =
  case parseJSON s of
    Left e -> print e
    Right j -> putStrLn
      (printJSON j)

pJSON :: Parser JSON
pJSON =
  fmap Number pInteger <|>
  fmap List pList <|>
  fmap Object pObject <|>
  fmap Str pString

test1 = parseJSON "15" ==
  Right (Number 15)

test2 = parseJSON "[1,2]" ==
  Right (List [Number 1, Number 2])

test3 = parseJSON "\"abc\"" ==
  Right (Str "abc")

test4 = parseJSON jsonInput ==
  Right (Object
    [("a", Number 5),
     ("b", List [Number 10,
                 Number 15])])
jsonInput =
  "{\"a\":5,\"b\":[10,15]}"

test = test1 && test2 && test3 && test4

printJSON :: JSON -> String
printJSON j = case j of
  Number n -> show n
  Str s -> show s
  List js ->
    "[" ++
    intercalate "," (fmap printJSON js)
    ++ "]"
  Object ps ->
    "{" ++
    intercalate "," (fmap printProp ps)
    ++ "}"

printProp :: (String, JSON) -> String
printProp (k, v) =
  show k ++ ":" ++ printJSON v

data Err =
  Err String |
  Tried Err Err
  deriving (Eq, Show)

data Parser a = P
  (String -> Either Err (a, String))

pAnyChar :: Parser Char
pAnyChar = P
  (\s -> case s of
    ""        -> Left
     (Err ("expected a character \
           \but got empty input"))
    (c : cs) -> Right (c, cs))

pFail :: String -> Parser a
pFail e = P (\_ -> Left (Err e))

pSatisfy ::
  (Char -> Bool) ->
  Parser Char
pSatisfy f = do
  c <- pAnyChar
  case f c of
    True  -> return c
    False -> empty

pChar :: Char -> Parser Char
pChar c =
  pAnyChar >>= \c' ->
  case c == c' of
    True  -> return c
    False -> pFail
      ("expected " ++ show c ++
       " but got " ++ show c')

pDigit :: Parser Integer
pDigit =
  pAnyChar >>= \c ->
    case c of
      '0' -> return 0
      '1' -> return 1
      '2' -> return 2
      '3' -> return 3
      '4' -> return 4
      '5' -> return 5
      '6' -> return 6
      '7' -> return 7
      '8' -> return 8
      '9' -> return 9
      _   -> pFail
        ("expected a decimal digit \
         \but got " ++ show c)

digitsToNumber :: [Integer] -> Integer
digitsToNumber =
  foldl (\a b -> a * 10 + b) 0

pInteger :: Parser Integer
pInteger =
  fmap digitsToNumber (some pDigit)

pList :: Parser [JSON]
pList = do
  pChar '['
  pListNil <|> pListCons

pListNil :: Parser [JSON]
pListNil = do
  pChar ']'
  return []

pListCons :: Parser [JSON]
pListCons = do
  j <- pJSON
  js <- do
    js <- many (do
      pChar ','
      pJSON)
    pChar ']'
    return js
  return (j:js)

pObject :: Parser [(String, JSON)]
pObject = do
  pChar '{'
  pObjectNil <|> pObjectCons

pObjectNil :: Parser [(String, JSON)]
pObjectNil = do
  pChar '}'
  return []

pObjectCons :: Parser [(String, JSON)]
pObjectCons = do
  j <- pProperty
  js <- do
    js <- many (do
      pChar ','
      pProperty)
    pChar '}'
    return js
  return (j:js)

pProperty :: Parser (String, JSON)
pProperty = do
  s <- pString
  pChar ':'
  j <- pJSON
  return (s, j)

pEsc :: Parser Char
pEsc = (do
  pChar '\\'
  (pChar '\\' <|>
   pChar '\"' <|>
   (fmap (const '\n') (pChar 'n'))))

pString :: Parser String
pString = do
  pChar '\"'
  s <- many (pEsc <|> pSatisfy (/='\"'))
  pChar '\"'
  return s

pChoice ::
  Parser a ->
  Parser a ->
  Parser a
pChoice p1 p2 = P (\s ->
  case runParser s p1 of
    Right r -> Right r
    Left  e ->
      case runParser s p2 of
        Right r -> Right r
        Left e' ->
          Left (Tried e e'))

pReturn :: a -> Parser a
pReturn a = P (\s -> Right (a, s))

pBind ::
  Parser a ->
  (a -> Parser b) ->
  Parser b
pBind pa f =
  P (\s ->
    case runParser s pa of
      Left e        -> Left e
      Right (a, s') ->
        runParser s' (f a)
    )

instance Functor Parser where
  fmap f m =
    m >>= \x -> return (f x)

instance Applicative Parser where
  pure = return
  ff <*> fa = do
    f <- ff
    a <- fa
    return (f a)

instance Alternative Parser where
  (<|>) = pChoice
  empty = pFail ""

instance Monad Parser where
  return = pReturn
  (>>=) = pBind

runParser ::
  String ->
  Parser a ->
  Either Err (a, String)
runParser s (P p) =
  p s
