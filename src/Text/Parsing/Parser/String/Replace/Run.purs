-- | # Running a Parser
-- |
-- | Functions in this module are *ways to run a parser* on a `String`,
-- | like `runParser` or `runParserT`.
-- |
-- | Note that these parser runners only accept the type `String`, not any
-- | instance of the `StringLike` class.
module Text.Parsing.Replace.String
  ( breakCap
  , breakCapT
  , splitCap
  , splitCapT
  , streamEdit
  , streamEditT
  )
where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (get)
import Data.Either (Either(..), hush)
import Data.List (List(..), manyRec, (:))
import Data.List.NonEmpty (fold1, singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.String (null)
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (T3, (/\))
import Text.Parsing.Parser (ParseState(..), Parser, ParserT, runParserT)
import Text.Parsing.Parser.String (anyChar)
import Text.Parsing.Parser.String.Combinator (anyTill)

-- | Monad transformer version of `breakCap`. The `sep` parser will run
-- | in the monad context.
breakCapT
  :: forall m a
   . (Monad m)
  => (MonadRec m)
  => ParserT String m a
  -> String
  -> m (Maybe (T3 String a String))
breakCapT sep input = hush <$> runParserT input go
 where
  go = do
    Tuple prefix cap <- anyTill sep
    ParseState suffix _ _ <- get
    pure $ prefix /\ cap /\ suffix

-- | #### Break on and capture one pattern
-- |
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
-- | - `Just (prefix /\ parse_result /\ suffix)` for the result of parsing the
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
-- | let (Just (prefix /\ (infix /\ _) /\ suffix)) = breakCap (match sep) input
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
  -> Maybe (T3 String a String)
breakCap sep input = unwrap $ breakCapT sep input

-- | Monad transformer version of `splitCap`. The `sep` parser will run in the
-- | monad context. Not stack-safe.
splitCapT
  :: forall m a
   . (Monad m)
  => (MonadRec m)
  => ParserT String m a
  -> String
  -> m (NonEmptyList (Either String a))
splitCapT sep input =
  runParse >>= case _ of
    Left _ -> pure $ singleton $ Left input
    Right (Tuple xs remain) -> do
      let final = if null remain then Nil else Cons (Left remain) Nil
      let
        go Nil = final
        go ((Tuple "" result) : rest) =
          Right result : go rest
        go ((Tuple prefix result) : rest) =
          Left prefix : Right result : go rest
      case xs of
        Nil -> pure $ singleton $ Left input
        Cons (Tuple "" result) rest -> do
          let rest' = go rest
          pure $ wrap $ Right result :| rest'
        Cons (Tuple prefix result) rest -> do
          let rest' = go rest
          pure $ wrap $ Left prefix :| Right result : rest'
 where
  runParse = runParserT input $ do
    xs <- manyRec (forceProgress $ anyTill sep)
    ParseState remain _ _ <- get
    pure $ Tuple xs remain
  -- If the parser succeeds but consumes no input, then force it to skip
  -- ahead one char.
  forceProgress p = do
    ParseState remain0 _ _ <- get
    x <- p
    ParseState remain1 _ _ <- get
    if CodeUnits.length remain0 == CodeUnits.length remain1
      then -- p succeeded but consumed nothing
        anyChar *> pure x
      else
        pure x




-- | #### Split on and capture all patterns
-- |
-- | Find all occurences of the pattern `sep`, split the input string, capture
-- | all the patterns and the splits.
-- |
-- | The input string will be split on every leftmost non-overlapping occurence
-- | of the pattern `sep`. The output list will contain
-- | the parsed result of input string sections which match the `sep` pattern
-- | in `Right`, and non-matching sections in `Left`.
-- |
-- | #### Access the matched section of text
-- |
-- | If you want to capture the matched strings, then combine the pattern
-- | parser `sep` with the `match` combinator.
-- |
-- | With the matched strings, we can reconstruct the input string.
-- | For all `input`, `sep`, if
-- |
-- | ```purescript
-- | let output = splitCap (match sep) input
-- | ```
-- |
-- | then
-- |
-- | ```purescript
-- | input == fold (either identity fst <$> output)
-- | ```
-- |
-- | (This invariant might not hold if `sep` can succeed without consuming
-- | any input, like if `sep` is a `lookAhead` parser.)
splitCap
  :: forall a
   . Parser String a
  -> String
  -> NonEmptyList (Either String a)
splitCap sep input = unwrap $ splitCapT sep input

-- | Monad transformer version of `streamEdit`. The `sep` parser and the
-- | `editor` function will both run in the monad context. Not stack-safe.
streamEditT
  :: forall m a
   . (Monad m)
  => (MonadRec m)
  => ParserT String m a
  -> (a -> m String)
  -> String
  -> m String
streamEditT sep editor input = do
  sections <- splitCapT sep input
  -- Is this fold1 efficient like mconcat?
  map fold1 $ for sections $ case _ of
    Left l -> pure l
    Right r -> editor r

-- |
-- | #### Stream editor
-- |
-- | Also known as “find-and-replace”, or “match-and-substitute”. Find all
-- | of the leftmost non-overlapping sections of the input string which match
-- | the pattern `sep`, and
-- | replace them with the result of the `editor` function.
-- |
-- | #### Access the matched section of text in the `editor`
-- |
-- | If you want access to the matched string in the `editor` function,
-- | then combine the pattern parser `sep`
-- | with `match`. This will effectively change
-- | the type of the `editor` function to `(String /\ a) -> String`.
-- |
-- | This allows us to write an `editor` function which can choose to not
-- | edit the match and just leave it as it is. If the `editor` function
-- | returns the first item in the tuple, then `streamEdit` will not change
-- | the matched string.
-- |
-- | So, for all `sep`:
-- |
-- | ```purescript
-- | streamEdit (match sep) fst ≡ identity
-- | ```
-- |
-- | (This invariant might not hold if `sep` can succeed without consuming
-- | any input, like if `sep` is a `lookAhead` parser.)
streamEdit
  :: forall a
   . Parser String a
  -> (a -> String)
  -> String
  -> String
streamEdit sep editor input = unwrap $ streamEditT sep (wrap <<< editor) input