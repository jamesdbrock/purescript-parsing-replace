# purescript-parsing-replace

[![Test](https://github.com/jamesdbrock/purescript-parsing-replace/workflows/Test/badge.svg?branch=main)](https://github.com/jamesdbrock/purescript-parsing-replace/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-parsing-replace/badge)](http://pursuit.purescript.org/packages/purescript-parsing-replace/)

__parsing-replace__ is for finding text patterns, and also
replacing or splitting on the found patterns.
This activity is traditionally done with regular expressions,
but __parsing-replace__ uses
[`Text.Parsing.Parser.String`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Text.Parsing.Parser.String)
parsers instead for the pattern matching.

__parsing-replace__ can be used in the same sort of __*“pattern capture”*__
or __*“find all”*__ situations in which one would use
[`Data.String.Regex.match`](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:match)
or
[`Data.String.Regex.search`](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:search).

__parsing-replace__ can be used in the same sort of __*“stream editing”*__
or __*“search-and-replace”*__ situations in which one would use
[`Data.String.Regex.replace'`](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:replace').

__parsing-replace__ can be used in the same sort of __*“string splitting”*__
situations in which one would use
[`Data.String.Regex.split`](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:split).

See [__replace-megaparsec__](https://hackage.haskell.org/package/replace-megaparsec)
for the Haskell
[__megaparsec__](http://hackage.haskell.org/package/megaparsec)
version.

See [__replace-attoparsec__](https://hackage.haskell.org/package/replace-attoparsec)
for the Haskell
[__attoparsec__](http://hackage.haskell.org/package/attoparsec)
version.

We can expect the performance of parser-based find-and-replace to be
worse than regex-based find-and-replace in a JavaScript
runtime environment. This module is intended for use when the input
size is modest, the pattern is complicated, and readability and
maintainability are more important than speed.

## Why would we want to do pattern matching and substitution with parsers instead of regular expressions?

* Monadic parsers have a nicer syntax than
  [regular expressions](https://en.wikipedia.org/wiki/Regular_expression),
  which are notoriously
  [difficult to read](https://en.wikipedia.org/wiki/Write-only_language).

* Regular expressions can do “group capture” on sections of the matched
  pattern, but they can only return stringy lists of the capture groups. Parsers
  can construct typed data structures based on the capture groups, guaranteeing
  no disagreement between the pattern rules and the rules that we're using
  to build data structures based on the pattern matches.

  For example, consider
  scanning a string for numbers. A lot of different things can look like a number,
  and can have leading plus or minus signs, or be in scientific notation, or
  have commas, or whatever. If we try to parse all of the numbers out of a string
  using regular expressions, then we have to make sure that the regular expression
  and the string-to-number conversion function agree about exactly what is
  and what isn't a numeric string. We can get into an awkward situation in which
  the regular expression says it has found a numeric string but the
  string-to-number conversion function fails. A typed parser will perform both
  the pattern match and the conversion, so it will never be in that situation.
  [Parse, don't validate.](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)

* Regular expressions are only able to pattern-match
  [regular](https://en.wikipedia.org/wiki/Chomsky_hierarchy#The_hierarchy)
  grammers.
  Monadic parsers are able pattern-match context-free grammers.

* The replacement expression for a traditional regular expression-based
  substitution command is usually just a string template in which
  the *Nth* “capture group” can be inserted with the syntax `\N`. With
  this library, instead of a template, we get
  an `editor` function which can perform any computation, including `Effect`s.

## Usage Examples

These usage examples are implemented in [`test/Main.purs`](test/Main.purs).

### Break strings with `breakCap`

Find the first pattern match and break the input string on the pattern.

```purescript
breakCap (string "needle") "hay needle hay"
```
```purescript
Just $ "hay " /\ "needle" /\ " hay"
```

### Split strings with `splitCap`

Split the input string on all pattern matches.

```purescript
splitCap (string "needle") "hay needle straw needle hay"
```
```
[Left "hay ", Right "needle", Left " straw ", Right "needle", Left " hay"]
```

### Edit strings with `streamEdit`

Edit all found patterns with an `editor` function.

```purescript
streamEdit (string "needle") toUpper "hay needle hay"
```
```purescript
"hay NEEDLE hay"
```

### Break strings with `breakCap` and `match`

Find the first pattern match, capture the matched text and the parsed result.

```purescript
parseInt = some digit >>= fromCharArray >>> fromString >>> maybe (fail "fromString") pure
breakCap (match parseInt) "abc 123 def"
```
```purescript
Just $ "abc " /\ ("123" /\ 123) /\ " def"
```

### Find all positions of a pattern with `splitCap`

Find the beginning positions of all pattern matches in the input.

```purescript
catMaybes $ hush <$> splitCap (position <* string "A") ".A...\n...A."
```
```purescript
[Position { line: 1, column: 2 }, Position { line: 2, column: 4 }]
```

### Edit strings in `Effect` in with `streamEditT`

Find an environment variable in curly braces and replace it with its value
from the environment.
We can read from the environment because `streamEditT` is running the
`editor` function in `Effect`.

```purescript
pattern = string "{" *> anyTill (string "}")
editor  = fst >>> lookupEnv >=> fromMaybe "" >>> pure
streamEditT pattern editor "◀ {HOME} ▶"
```
```purescript
"◀ /home/jbrock ▶"
```

### Count the pattern matches with `splitCapT`

Parse in a `State` monad to remember state in the parser. This
stateful `letterCount` parser counts
the number of pattern matches which occur in the input, and also
tags each match with its index.

```purescript
letterCount :: ParserT String (State Int) (Tuple Char Int)
letterCount = do
  l <- letter
  i <- lift $ modify (_+1)
  pure $ l /\ i

flip runState 0 $ splitCapT letterCount "A B"
```
```purescript
[Right ('A' /\ 1), Left " ", Right ('B' /\ 2)] /\ 2
```


## Alternatives

* https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex
* https://pursuit.purescript.org/packages/purescript-substitute/

## Is this a good idea?

You may have
[heard it suggested](https://stackoverflow.com/questions/57667534/how-can-i-use-a-parser-in-haskell-to-find-the-locations-of-some-substrings-in-a/57712672#comment101804063_57667534)
that monadic parsers are better for pattern-matching when
the input stream is mostly signal, and regular expressions are better
when the input stream is mostly noise.

The premise of this library is that monadic parsers are great for finding
small signal patterns in a stream of otherwise noisy text.

Our reluctance to forego the speedup opportunities afforded by restricting
ourselves to regular grammars is an old superstition about
opportunities which
[remain mostly unexploited anyway](https://swtch.com/~rsc/regexp/regexp1.html).
The performance compromise of allowing stack memory allocation (a.k.a. pushdown
automata, a.k.a. context-free grammar) was once considered
[controversial for *general-purpose* programming languages](https://vanemden.wordpress.com/2014/06/18/how-recursion-got-into-programming-a-comedy-of-errors-3/).
I think we
can now resolve that controversy the same way for pattern matching languages.