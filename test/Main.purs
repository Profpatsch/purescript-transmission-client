module Test.Main ( main ) where

import Prelude

import Effect (Effect)
import Effect.Console

import Data.Array

import Main as Main

-- import Dhall.Generic as DG

main :: Effect Unit
main = do
  -- Main.main

  -- foo :: ( i :: Int, i :: Boolean )
  log "hello"
  log (show $ labelPlusOne { foo: 42, bar: "hi" })
  pure unit

labelPlusOne :: forall r a. { foo :: Int | r } -> (a -> Int)
             -> { foo :: Int | r }
labelPlusOne rVal = rVal { foo = rVal.foo + 1 }

id :: forall a. a -> a
id a = a
