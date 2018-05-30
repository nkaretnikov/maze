module Main where

import qualified Reflex.Dom as RD

import           Canvas

main :: IO ()
main = RD.mainWidget bodyElement
