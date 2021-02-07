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
module Text.Parsing.Parser.String.Replace.Run
  ( breakCap
  , breakCapT
  )
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
-- | #### Output
-- |
-- | - `Nothing` when no pattern match was found.
-- | - `Just (prefix, parse_result, suffix)` for the result of parsing the
-- |   pattern match, and the `prefix` string before and the `suffix` string
-- |   after the pattern match. `prefix` and `suffix` may be zero-length strings.
-- |
-- | #### Access the matched section of text
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