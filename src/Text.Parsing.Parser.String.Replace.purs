-- | Finding and replacing `String`s with
-- | [`Text.Parsing.Parser.String`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Text.Parsing.Parser.String)
-- | instead of
-- | [`Data.String.Regex`](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex).
-- |
-- | We can expect the performance of parser-based find-and-replace to be
-- | noticeably worse than regex-based find-and-replace in a JavaScript
-- | runtime environment. This module is intended for use when the input
-- | size is modest, the pattern is complicated, and readability and
-- | maintainability are more important than speed.
module Text.Parsing.Parser.String.Replace
  (match)
where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (get)
import Control.Plus ((<|>))
import Data.Either (hush)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList, cons')
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)
import Text.Parsing.Parser (ParseState(..), ParserT, Parser, runParserT)
import Text.Parsing.Parser.String (anyChar)

-- | Return both the result of a parse and the portion of the input that
-- | was consumed while it was being parsed.
-- |
-- | Note that this combinator operates only on `String`, not on anything that
-- | is `StringLike`.
match :: forall m a. Monad m => ParserT String m a -> ParserT String m (Tuple String a)
match p = do
  ParseState input1 _ _ <- get
  x <- p
  ParseState input2 _ _ <- get
  pure $ Tuple (CodeUnits.take (CodeUnits.length input1 - CodeUnits.length input2) input1) x

-- | Parse several phrases until the specified terminator matches.
-- | Returns the list of phrases and the terminator.
manyTill_ :: forall s a m e. Monad m => ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (List a) e)
manyTill_ p end = scan
  where
    scan =
      do
        t <- end
        pure $ Tuple Nil t
      <|>
      do
        x <- p
        Tuple xs t <- scan
        pure $ Tuple (x:xs) t

-- | Parse several phrases until the specified terminator matches, requiring at least one match.
-- | Returns the list of phrases and the terminator.
many1Till_ :: forall s a m e. Monad m => ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (NonEmptyList a) e)
many1Till_ p end = do
  x <- p
  Tuple xs t <- manyTill_ p end
  pure $ Tuple (cons' x xs) t

-- | Find the first place in the input where the phrase matches. Returns both the
-- | match and the input section consumed preceding the match.
-- |
-- | Note that this combinator operates only on `String`, not on anything that
-- | is `StringLike`.
-- |
-- | This combinator is equivalent to `manyTill_ anyChar` but it will be much
-- | faster because it returns a slice of the input `String` for the
-- | section preceding the match instead of a `List Char`.
anyTill :: forall m a. (Monad m) => (MonadRec m) => ParserT String m a -> ParserT String m (Tuple String a)
anyTill p = do
  ParseState input1 _ _ <- get
  Tuple input2 t <- tailRecM go unit
  pure $ Tuple (CodeUnits.take (CodeUnits.length input1 - CodeUnits.length input2) input1) t
 where
  go unit =
    do
      ParseState input2 _ _ <- get
      t <- p
      pure $ Done $ Tuple input2 t
    <|>
    do
    -- Why does `anyChar` use `CodeUnit.uncons`?
    -- https://github.com/purescript-contrib/purescript-parsing/issues/109
      _ <- anyChar
      pure $ Loop unit

-- | Monad transformer version of `breakCap`.
breakCapT
  :: forall m a
   . (Monad m)
  => (MonadRec m)
  => ParserT String m a
  -> String
  -> m (Maybe (Tuple3 String a String))
breakCapT sep input = hush <$> runParserT input go
 where
  go = do
    Tuple prefix cap <- anyTill sep
    ParseState suffix _ _ <- get
    pure $ tuple3 prefix cap suffix

-- | Find the first occurence of a pattern in a text stream, capture the found
-- | pattern, and break the input text stream on the found pattern.
-- |
-- | Be careful not to look too far
-- | ahead; if the `sep` parser looks to the end of the input then `breakCap`
-- | could be *O(n²)*.
-- |
-- | ## Output
-- |
-- | - `Nothing` when no pattern match was found.
-- | - `Just (prefix, parse_result, suffix)` for the result of parsing the
-- |   pattern match, and the `prefix` string before and the `suffix` string
-- |   after the pattern match. `prefix` and `suffix` may be zero-length strings.
-- |
-- | ## Access the matched section of text
-- |
-- | If you want to capture the matched string, then combine the pattern
-- | parser `sep` with `match`.
-- |
-- | With the matched string, we can reconstruct the input string.
-- | For all `input`, `sep`, if
-- |
-- | ```purescript
-- | let (Just (prefix, (infix, _), suffix)) = breakCap (match sep) input
-- | ```
-- |
-- | then
-- |
-- | ```purescript
-- | input == prefix <> infix <> suffix
-- | ```
breakCap
  :: forall a
   . Parser String a
  -> String
  -> Maybe (Tuple3 String a String)
breakCap sep input = unwrap $ breakCapT sep input