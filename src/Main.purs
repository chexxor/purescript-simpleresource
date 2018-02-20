module SimpleResource where

class SimpleResource m k a where
  getResource :: k -> m a
