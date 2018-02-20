module Test.Main where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (Storage, getItem, setItem)
import Data.Array (filter)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Global (readFloat)
import SimpleResource (class SimpleResource, getResource)


data User = User { firstName :: String, lastName :: String }
instance userShow :: Show User where
  show (User u) = "(User " <> u.firstName <> " " <> u.lastName <> " )"
derive instance genericUser :: Generic User _
instance encodeUser :: Encode User where
  encode = genericEncode defaultOptions
instance decodeUser :: Decode User where
  decode = genericDecode defaultOptions
data UserRequest = UserRequest String -- firstName
derive instance genericUserRequest :: Generic UserRequest _
instance encodeUserRequest :: Encode UserRequest where
  encode = genericEncode defaultOptions
instance decodeUserRequest :: Decode UserRequest where
  decode = genericDecode defaultOptions
data UserResponse =
  Success { data :: Array User }
  | Failure { error :: String }
derive instance genericUserResponse :: Generic UserResponse _
instance encodeUserResponse :: Encode UserResponse where
  encode = genericEncode defaultOptions
instance decodeUserResponse :: Decode UserResponse where
  decode = genericDecode defaultOptions

dummyUserDB :: Array User
dummyUserDB =
  [ User { firstName: "John", lastName: "Doe" }
  , User { firstName: "Jane", lastName: "Doe" }
  , User { firstName: "Jane", lastName: "Goodacre" }
  , User { firstName: "Jimmy", lastName: "Heathen" }
  ]

instance userSimpleResource ::
    (MonadEff eff m) =>
    SimpleResource m UserRequest UserResponse where
  getResource (UserRequest s) =
    filter (\(User u) -> u.firstName == s) dummyUserDB
    # case _ of
      [] ->
        pure (Failure { error: "No users found!" })
      us -> 
        pure (Success { data: us })


newtype CachedLS r = CachedLS r
derive instance newtypeCachedLS :: Newtype (CachedLS r) _

instance cachedSimpleResource ::
    ( MonadEff (now :: NOW, dom :: DOM | eff) m
    , Encode k, Decode k, Encode a, Decode a
    , SimpleResource m k a) =>
    SimpleResource m k (CachedLS a) where
  getResource k =
    let
      serializedK = encodeJSON k
      serializedKTS = serializedK <> "_ts"
      nowMs :: Eff (now :: NOW, dom :: DOM | eff) Number
      nowMs = now <#> unwrap <<< unInstant
      maxAge = 7 * 24 * 60 * 60 * 1000 -- 1 week in ms
      getAndCacheFreshResource :: Storage -> Number -> k -> m a
      getAndCacheFreshResource s ts k' =
        getResource k' >>= \a -> do
          liftEff (setItem serializedK (encodeJSON a) s)
          liftEff (setItem serializedKTS (show ts) s)
          pure a
    in do
      nowMs' <- liftEff nowMs
      s <- liftEff $ window >>= localStorage
      cachedVal :: Maybe a <- liftEff $ getItem serializedK s
        <#> bindFlipped (either (const Nothing) Just <<< runExcept <<< decodeJSON)
      cachedValTS <- liftEff $ getItem serializedKTS s <#> (map readFloat)
      CachedLS <$> case cachedVal, cachedValTS of
        Just cachedVal', Just cachedVal'TS -> do
          if nowMs' - cachedVal'TS < toNumber maxAge
            then pure cachedVal'
            else getAndCacheFreshResource s nowMs' k
        _, _ ->
          getAndCacheFreshResource s nowMs' k


main :: Eff (console :: CONSOLE, dom :: DOM, now :: NOW) Unit
main = do
  res :: UserResponse <- getResource (UserRequest "Jane")
  case res of
    Success s -> logShow s.data
    Failure f -> log f.error
  res :: UserResponse <- getResource (UserRequest "Mark")
  case res of
    Success s -> logShow s.data
    Failure f -> log f.error
  res :: CachedLS UserResponse <- getResource (UserRequest "Jane")
  case unwrap res of
    Success s -> logShow s.data
    Failure f -> log f.error
  pure unit