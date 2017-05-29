module Lambs.Parse (parseTop, removeComment) where

import Data.Array as Array
import Data.Set as Set
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.List (List(..), foldl, many, (:))
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import Lambs.Define (Define(..), Undefine(..))
import Lambs.Term (Term(..))
import Lambs.Toplevel (Toplevel(..))
import Prelude (class Monad, Unit, bind, pure, unit, (*), (+), (/=), (<<<), (>>=))
import Text.Parsing.Parser (Parser, ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (between, optional)
import Text.Parsing.Parser.String (class StringLike, eof, satisfy, string)
import Text.Parsing.Parser.Token (alphaNum, letter)

c :: forall a b.a -> b -> Parser String a
c n _ = pure n

reserved :: Set.Set Char
reserved = Set.fromFoldable [':', '\\', '≜', 'λ', ' ', '\n', '\t', '.']

many1 :: forall a b. Parser a b -> Parser a (List b)
many1 p = many p >>= halp
  where
    halp Nil = fail "oh no"
    halp l = pure l

listString :: List Char -> String
listString l = fromCharArray (Array.fromFoldable l)

identifier :: Parser String String
identifier = many1 alphaNum >>= pure <<< listString

letters :: Parser String String
letters = many1 letter >>= pure <<< listString

digit :: Parser String Int
digit =
  (string "0" >>= c 0)
  <|> (string "1" >>= c 1)
  <|> (string "2" >>= c 2)
  <|> (string "3" >>= c 3)
  <|> (string "4" >>= c 4)
  <|> (string "5" >>= c 5)
  <|> (string "6" >>= c 6)
  <|> (string "7" >>= c 7)
  <|> (string "8" >>= c 8)
  <|> (string "9" >>= c 9)

number :: Parser String Int
number = many1 digit >>= (\l -> pure (foldl (\res n -> n + (res * 10)) 0 l))

lam :: Parser String Term -> Parser String Term
lam x = do
  _ <- (string "λ") <|> (string "\\")
  p <- identifier
  _ <- (string ".")
  b <- x
  pure (Lam p b)

var :: Parser String Term
var = identifier >>= pure <<< Var

whiteChar :: Parser String Unit
whiteChar = (string " " <|> string "\t" <|> string "\n") >>= c unit

white :: Parser String Unit
white = many whiteChar >>= c unit

white1 :: Parser String Unit
white1 = many1 whiteChar >>= c unit

notApp :: Parser String Term -> Parser String Term
notApp x = paren x <|> lam x <|> var

exp ::  Parser String Term
exp = fix (\self -> many (notAppW self) >>= applify)
  where
    notAppW ex = do
      t <- notApp ex
      _ <- white
      pure t
    applify Nil = fail "want more than zero expressions :("
    applify (hd : tl) = pure (foldl App hd tl)

paren :: forall a b m. (StringLike a, Monad m) => ParserT a m b -> ParserT a m b
paren x = between (string "(") (string ")") x

term :: Parser String Term
term = do
  x <- exp
  _ <- white
  _ <- eof
  pure x


def :: Parser String Define
def = do
  _ <- white
  name <- identifier
  _ <- white
  _ <- string "≜" <|> string ":="
  _ <- white
  t <- term
  pure (Define name t)

undef :: Parser String Undefine
undef = do
  _ <- white
  name <- identifier
  _ <- white
  _ <- string "≜" <|> string ":="
  _ <- white
  _ <- optional (string ":(")
  _ <- white
  _ <- eof
  pure (Undefine name)

run :: forall a b. (Parser a b) -> a -> Maybe b
run p x =
  case runParser x p of
    Right y -> Just y
    Left y -> Nothing

parse :: String -> Maybe Term
parse = run term

parseTop :: String -> Maybe Toplevel
parseTop s =
  case run undef s of
    Just u -> Just (Undef u)
    Nothing ->
      case run def s of
        Just d -> Just (Def d)
        Nothing ->
          case run term s of
            Just t -> Just (Trm t)
            Nothing -> Nothing

removeComment :: String -> String
removeComment s =
  case runParser s remove of
    Left _ -> s
    Right res -> res

    where
      remove = many (satisfy (\x -> x /= '|')) >>= (\l -> pure (listString l))
