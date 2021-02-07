-- | # Parser Combinators
-- |
-- | Function in this module are *parser combinators*.
module Text.Parsing.Parser.String.Replace.Combinator
  ( match
  , anyTill
  , manyTill_
  , many1Till_
  )
where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (get)
import Control.Plus ((<|>))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList, cons')
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParseState(..), ParserT)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (anyChar)

-- | Return both the result of a parse and the portion of the input that
-- | was consumed while it was being parsed.
-- |
-- | Note that this combinator only accepts the type `String`, not any instance
-- | of the `StringLike` class.
match :: forall m a. Monad m => ParserT String m a -> ParserT String m (Tuple String a)
match p = do
  ParseState input1 _ _ <- get
  x <- p
  ParseState input2 _ _ <- get
  -- We use the Javascript `length`, which is in
  -- [units of “code units”](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length),
  -- instead of `Data.String.length`. which is in
  -- [units of “code points”](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String#v:length).
  -- This is more efficient, and it will be correct as long as we can assume
  -- that the `ParseState input` always begins on a code point boundary.
  pure $ Tuple (CodeUnits.take (CodeUnits.length input1 - CodeUnits.length input2) input1) x

-- | Find the first place in the input where the phrase can parse. Returns both the
-- | parsed result and the unparsable input section consumed before the parse.
-- | Will fail if no section of the input is parseable. Will not consume input
-- | on failure. Stack-safe.
-- |
-- | This combinator is equivalent to `manyTill_ anyChar` but it will be
-- | faster because it returns a slice of the input `String` for the
-- | section preceding the match instead of a `List Char`.
-- |
-- | Note that this combinator only accepts the type `String`, not any instance
-- | of the `StringLike` class.
anyTill :: forall m a. (Monad m) => (MonadRec m) => ParserT String m a -> ParserT String m (Tuple String a)
anyTill p = try $ do
  ParseState input1 _ _ <- get
  Tuple input2 t <- tailRecM go unit
  pure $ Tuple (CodeUnits.take (CodeUnits.length input1 - CodeUnits.length input2) input1) t
 where
  go unit =
    do
      ParseState input2 _ _ <- get
      t <- try $ p
      pure $ Done $ Tuple input2 t
    <|>
    do
    -- Why does `anyChar` use `CodeUnit.uncons`?
    -- https://github.com/purescript-contrib/purescript-parsing/issues/109
      _ <- anyChar
      pure $ Loop unit

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