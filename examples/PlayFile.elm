module PlayFile where

import Graphics.Element exposing (Element)
import Graphics.Input exposing (button)
import WebAudio exposing (..)
import Window

type alias State = { playing : Bool }

type Action
    = NoOp
    | Play
    | Pause

defaultState = { playing = False }

makeGain x =
    let node = createGainNode DefaultContext
        _ = setValue 0.5 node.gain
    in node |> connectNodes (getDestinationNode DefaultContext) 0 0

defaultGainNode = makeGain 0.8

makeStream url =
    createHiddenMediaElementAudioSourceNode DefaultContext
        |> setMediaElementIsLooping True
        |> setMediaElementSource url
        |> connectNodes defaultGainNode 0 0

streamNode = makeStream "shepards.mp3"

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

view : (Int, Int) -> State -> Element
view (w,h) state =
    let label = if state.playing then "Pause" else "Play"
        action = if (not state.playing) then Play else Pause
        msg = Signal.message actions.address action
    in button msg label

-- Updates
update : Action -> State -> State
update action state =
    case action of
      Play ->
          let _ = playMediaElement streamNode
          in { state | playing <- True }
      Pause ->
          let _ = pauseMediaElement streamNode
          in { state | playing <- False }
      NoOp -> state

-- Signals

state : Signal State
state = Signal.foldp update defaultState actions.signal

-- Main

main = Signal.map2 view Window.dimensions state