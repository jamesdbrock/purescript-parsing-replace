module Test.Main where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (Either(..), either, hush)
import Data.List (List(..), fold, (:))
import Data.List.NonEmpty (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (get2, (/\))
import Effect (Effect)
import Test.Assert (assertEqual')
import Text.Parsing.Parser (position, runParser)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.String.Replace (anyTill, breakCap, match, splitCap, streamEdit)


main :: Effect Unit
main = do
  assertEqual' "anyTill1"
    { actual: runParser "Baaaa" $ anyTill $ string "B"
    , expected: Right $ Tuple "" "B"
    }
  assertEqual' "anyTill2"
    { actual: runParser "aaBaa" $ anyTill $ string "B"
    , expected: Right $ Tuple "aa" "B"
    }
  assertEqual' "anyTill3"
    { actual: runParser "aaaaB" $ anyTill $ string "B"
    , expected: Right $ Tuple "aaaa" "B"
    }
  assertEqual' "breakCap1"
    { actual: breakCap (string "B") "Baaaa"
    , expected: Just $ "" /\ "B" /\ "aaaa"
    }
  assertEqual' "breakCap2"
    { actual: breakCap (string "B") "aaBaa"
    , expected: Just $ "aa" /\ "B" /\ "aa"
    }
  assertEqual' "breakCap3"
    { actual: breakCap (string "B") "aaaaB"
    , expected: Just $ "aaaa" /\ "B" /\ ""
    }
  assertEqual' "breakCap3"
    { actual: breakCap (string "B") ""
    , expected: Nothing
    }
  assertEqual' "breakCap4"
    { actual: breakCap (lookAhead $ string "B") "aaBaa"
    , expected: Just $ "aa" /\ "B" /\ "Baa"
    }
  assertEqual' "breakCap5"
    { actual: breakCap (match $ string "B") "aaBaa"
    , expected: Just $ "aa" /\ ("B" /\ "B") /\ "aa"
    }
  assertEqual' "breakCap6"
    { actual: breakCap (match $ lookAhead $ string "B") "aaBaa"
    , expected: Just $ "aa" /\ ("" /\ "B") /\ "Baa"
    }
  assertEqual' "splitCap1"
    { actual: splitCap (string "B") "BaB"
    , expected: wrap $ Right "B" :| Left "a" : Right "B" : Nil
    }
  assertEqual' "splitCap2"
    { actual: splitCap (string "B") "aBaB"
    , expected: wrap $ Left "a" :| Right "B" : Left "a" : Right "B" : Nil
    }
  assertEqual' "splitCap3"
    { actual: splitCap (string "B") "aBaBa"
    , expected: wrap $ Left "a" :| Right "B" : Left "a" : Right "B" : Left "a" : Nil
    }
  assertEqual' "splitCap4"
    { actual: splitCap (string "B") "a"
    , expected: wrap $ Left "a" :| Nil
    }
  assertEqual' "splitCap5"
    { actual: splitCap (string "B") ""
    , expected: wrap $ Left "" :| Nil
    }
  assertEqual' "splitCap6"
    { actual: splitCap (string "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯") "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"
    , expected: wrap $ Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :| Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯" : Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" : Nil
    }
  assertEqual' "splitCap7"$ let output = splitCap (match $ string "B") "aBaBa" in
    { actual: fold (either identity fst <$> output)
    , expected: "aBaBa"
    }
  assertEqual' "streamEdit1"
    { actual: streamEdit (match $ string "B") fst "aBa"
    , expected: "aBa"
    }
  assertEqual' "streamEdit2"
    { actual: streamEdit (string "B") (const "C") "aBa"
    , expected: "aCa"
    }
  assertEqual' "example1"
    { actual: show $ map get2 $ breakCap (position <* string "A") "..A.."
    , expected: "(Just (Position { line: 1, column: 3 }))"
    }
  assertEqual' "example2"
    { actual: show $ fromFoldable $ catMaybes $ hush <$> splitCap (position <* string "A") ".A...\n...A."
    , expected: "[(Position { line: 1, column: 2 }),(Position { line: 2, column: 4 })]"
    }