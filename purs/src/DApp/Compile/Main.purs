module DApp.Compile.Main (main) where

import Prelude

import Chanterelle (compileMain)
import Effect (Effect)

main :: Effect Unit
main = compileMain
