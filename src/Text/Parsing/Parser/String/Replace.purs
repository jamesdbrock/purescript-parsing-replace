-- | Finding and replacing `String`s with
-- | [`Text.Parsing.Parser.String`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Text.Parsing.Parser.String)
-- | instead of
-- | [`Data.String.Regex`](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex).
-- |
-- | See the package README for usage examples.
module Text.Parsing.Parser.String.Replace
  ( module Text.Parsing.Parser.String.Replace.Combinator
  , module Text.Parsing.Parser.String.Replace.Run
  )
where

import Text.Parsing.Parser.String.Replace.Combinator (anyTill, match, manyTill_, many1Till_)
import Text.Parsing.Parser.String.Replace.Run (breakCap, breakCapT, splitCap, splitCapT, streamEdit, streamEditT)
