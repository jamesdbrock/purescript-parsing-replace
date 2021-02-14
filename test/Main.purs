module Test.Main where

import Prelude

import Control.Monad.State (State, lift, modify, runState)
import Data.Array (fromFoldable, some)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Int (fromString)
import Data.List (List(..), fold, (:))
import Data.List.NonEmpty (catMaybes)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (get2, (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Process (lookupEnv)
import Test.Assert (assertEqual')
import Text.Parsing.Parser (ParserT, fail, position, runParser)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Replace (anyTill, breakCap, match, splitCap, splitCapT, streamEdit, streamEditT)
import Text.Parsing.Parser.Token (digit, letter)


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
  assertEqual' "example0"
    { actual:
        let parseInt = some digit >>= fromCharArray >>> fromString >>> maybe (fail "fromString") pure
        in
        breakCap (match parseInt) "abc 123 def"
    , expected: Just $ "abc " /\ ("123" /\ 123) /\ " def"
    }
  assertEqual' "example1"
    { actual: show $ map get2 $ breakCap (position <* string "A") "..A.."
    , expected: "(Just (Position { line: 1, column: 3 }))"
    }
  assertEqual' "example2"
    { actual: show $ fromFoldable $ catMaybes $ hush <$> splitCap (position <* string "A") ".A...\n...A."
    , expected: "[(Position { line: 1, column: 2 }),(Position { line: 2, column: 4 })]"
    }
  -- This test doesn't pass in CI.
  -- assertEqual' "example3"
  --   { actual: unsafePerformEffect $ streamEditT (string "{" *> anyTill (string "}")) (fst >>> lookupEnv >=> fromMaybe "" >>> pure) "◀ {HOME} ▶"
  --   , expected: "◀ /home/jbrock ▶"
  --   }
  assertEqual' "example4"
    { actual:
        let letterCount :: ParserT String (State Int) (Tuple Char Int)
            letterCount = do
              l <- letter
              i <- lift $ modify (_+1)
              pure $ l /\ i
        in
        lmap fromFoldable $ flip runState 0 $ splitCapT letterCount "A B"
    , expected: [(Right ('A' /\ 1)),(Left " "),(Right ('B' /\ 2))] /\ 2
    }