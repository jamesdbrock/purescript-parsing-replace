{-
-}
{ name = "parsing-replace"
, dependencies =
  [ "control"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "parsing"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/jamesdbrock/purescript-parsing-replace"
}
