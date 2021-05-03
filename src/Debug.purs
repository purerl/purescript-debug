module Debug
  ( class DebugWarning
  , trace
  , traceM
  , spy
  , spyWith
  ) where

import Prelude
import Prim.TypeError (class Warn, Text)

-- | Nullary class used to raise a custom warning for the debug functions.
class DebugWarning

instance warn :: Warn (Text "Debug function usage") => DebugWarning

-- | Log any PureScript value to the console for debugging purposes and then
-- | return a value. This will log the value's underlying representation for
-- | low-level debugging, so it may be desireable to `show` the value first.
-- |
-- | The return value is thunked so it is not evaluated until after the
-- | message has been printed, to preserve a predictable console output.
-- |
-- | For example:
-- | ``` purescript
-- | doSomething = trace "Hello" \_ -> ... some value or computation ...
-- | ```
trace :: forall a b. DebugWarning => a -> (Unit -> b) -> b
trace a k = _trace a k

foreign import _trace :: forall a b. a -> (Unit -> b) -> b

-- | Log any PureScript value to the console and return the unit value of the
-- | Monad `m`.
traceM :: forall m a. DebugWarning => Monad m => a -> m Unit
traceM s = do
  pure unit
  trace s \_ -> pure unit

-- | Logs any value and returns it, using a "tag" or key value to annotate the
-- | traced value. Useful when debugging something in the middle of a
-- | expression, as you can insert this into the expression without having to
-- | break it up.
spy :: forall a. DebugWarning => String -> a -> a
spy tag a = _spy tag a

foreign import _spy :: forall a. String -> a -> a

-- | Similar to `spy`, but allows a function to be passed in to alter the value
-- | that will be printed. Useful in cases where the raw printed form of a value
-- | is inconvenient to read - for example, when spying on a `Set`, passing
-- | `Array.fromFoldable` here will print it in a more useful form.
spyWith ∷ ∀ a b. DebugWarning ⇒ String → (a → b) → a → a
spyWith msg f a = const a (spy msg (f a))
