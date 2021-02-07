-- | # Running a Parser
-- |
-- | Functions in this module are *ways to run a parser* on a `String`,
-- | like `runParser` or `runParserT`.
-- |
-- | Note that these parser runners only accept the type `String`, not any
-- | instance of the `StringLike` class.
module Text.Parsing.Parser.String.Replace.Run
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
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)
import Text.Parsing.Parser (ParseState(..), Parser, ParserT, runParserT)
import Text.Parsing.Parser.String.Replace.Combinator (anyTill)

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

-- | Monad transformer version of `splitCap`.
-- | Not stack-safe.
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
    xs <- manyRec (anyTill sep)
    ParseState remain _ _ <- get
    pure $ Tuple xs remain

-- There's a problem: The ParseState Position is wrong.
-- splitCapT sep input = go input
--  where
--   go input' = breakCapT sep input >>= case _ of
--     Nothing ->
--       pure $ singleton $ Left input'
--     Just ("" /\ parse_result /\ "" /\ unit) ->
--       pure $ singleton $ Right parse_result
--     Just (prefix /\ parse_result /\ "" /\ unit) ->
--       pure $ cons (Left prefix) $ singleton (Right parse_result)
--     Just ("" /\ parse_result /\ suffix /\ unit) ->
--       map (cons (Right parse_result)) $ go suffix
--     Just (prefix /\ parse_result /\ suffix /\ unit) ->
--       map (cons (Left prefix) <<< cons (Right parse_result)) $ go suffix

-- |
splitCap
  :: forall a
   . Parser String a
  -> String
  -> NonEmptyList (Either String a)
splitCap sep input = unwrap $ splitCapT sep input

-- | Monad transformer version of `streamEdit`.
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
  map fold1 $ for sections $ case _ of
    Left l -> pure l
    Right r -> editor r

-- |
streamEdit
  :: forall a
   . Parser String a
  -> (a -> String)
  -> String
  -> String
streamEdit sep editor input = unwrap $ streamEditT sep (wrap <<< editor) input