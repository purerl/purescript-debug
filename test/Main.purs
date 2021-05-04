module Test.Main where

import Prelude
import Debug (spy, spyWith, trace, traceM)
import Effect (Effect)

main :: Effect Unit
main = do
  trace "Testing" \_ ->
    trace true \_ ->
      trace { x: 10 } \_ -> do
        traceM "Testing"
  traceM "Testing"
  effInt
    >>= spy "i"
    >>> eatInt
  effRec
    >>= spy "r"
    >>> \r -> traceM r.x
  void $ spyWith "x" _.x <$> effRec
  let
    dummy = spy "dummy" { foo: 1, bar: [ 1, 2 ] }
  traceM dummy
  traceM "³€½²³æßðđ"
  where
  effInt :: Effect Int
  effInt = pure 0

  effRec :: Effect { x :: String }
  effRec = pure { x: "foo" }

  eatInt :: Int -> Effect Unit
  eatInt = const $ pure unit
