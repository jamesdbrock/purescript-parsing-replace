-- | Finding and replacing `String`s with
-- | [`Text.Parsing.Parser.String`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Text.Parsing.Parser.String)
-- | instead of
-- | [`Data.String.Regex`](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex).
-- |
-- | See the package README for usage examples.
module Text.Parsing.Replace
  ( module Text.Parsing.Replace.String
  , module Text.Parsing.Parser.String.Combinator
  )
where

import Text.Parsing.Replace.String (breakCap, breakCapT, splitCap, splitCapT, streamEdit, streamEditT)
import Text.Parsing.Parser.String.Combinator (anyTill, match, manyTill_, many1Till_)
