module Test where

import WebAudio exposing (..)
import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (display)
import Random

gainNode = createGainNode DefaultContext

eps = 1e-5

approxEq x y = abs (x - y) < eps

(=~) x y = approxEq

claim_get_set_value =
  claim "getValue (setValue x param) =~ x"
    `true`
      (\x -> setValue x gainNode.gain >>= getValue =~ x)
    `for`
       float

basicSuite =
    suite "Basic functions"
              [ claim_get_set_value
              ]

allTests = quickCheck basicSuite

main = display allTests