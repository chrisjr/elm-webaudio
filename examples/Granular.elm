module Granular where

import Debug
import Graphics.Element exposing (Element, show)
import Maybe exposing (withDefault)
import Mouse
import Random exposing (Seed, Generator, generate, list, pair, float)
import Signal exposing ((<~), (~))
import Task exposing (Task)
import Time exposing (Time, millisecond, every)
import WebAudio exposing (..)
import Window

type alias Envelope =
    { attack : Float
    , release : Float
    }

type Panning
    = NoPan
    | Pan Float   -- [-1, 1]

type alias GrainParams =
    { transposition : Float
    , pan : Panning
    , bufferOffset : Float
    , startOffset : Float
    , amp : Float
    , envelope : Envelope
    }

type alias State = { buffer : Maybe AudioBuffer
                   , envelope : Envelope
                   , seed : Seed
                   , bufferSpread : Float
                   , triggerSpread : Float
                   , mousePos : (Int, Int)
                   , dims : (Int, Int)
                   }

type Action
    = NoOp
    | NewEnv Envelope
    | NewSeed Seed
    | BufferLoaded AudioBuffer

defaultEnvelope : Envelope
defaultEnvelope = { attack = 0.4, release = 0.4 }

defaultState : State
defaultState = { buffer = Nothing
               , envelope = defaultEnvelope
               , seed = Random.initialSeed 0
               , bufferSpread = 0.5
               , triggerSpread = 0.2
               , mousePos = (0, 0)
               , dims = (1,1) }

defaultGrainParams : GrainParams
defaultGrainParams =
    { transposition = 1.0
    , pan = NoPan
    , bufferOffset = 0.0
    , startOffset = 0.0
    , amp = 0.8
    , envelope = defaultEnvelope
    }

makeGain x =
    createGainNode DefaultContext
    |> tapNode .gain (setValue x)

defaultGainNode = makeGain 0.7
                |> connectNodes (getDestinationNode DefaultContext) 0 0

mulParam : Float -> AudioParam -> AudioParam
mulParam mul param = setValue (mul * getValue param) param

totalTime {attack, release} = attack + release

isJust : Maybe a -> Bool
isJust x = case x of
             Just _ -> True
             Nothing -> False

makeLinearPanner : Float -> PannerNode
makeLinearPanner x =
    createPannerNode DefaultContext
        |> setPanningModel EqualPower
        |> setDistanceModel Linear
        |> setPosition x 0.0 0.0

makePanner pan =
    case pan of
      Pan x -> Just <| makeLinearPanner x
      NoPan -> Nothing

maybeConnect : AudioNode a -> Maybe (AudioNode b) -> AudioNode c -> AudioNode a
maybeConnect a b c =
    case b of
      Just b' -> a |> connectNodes b' 0 0 |> connectNodes c 0 0
      Nothing -> a |> connectNodes c 0 0

startSourceAndEnv source localGain envelope bufferOffset startOffset amp =
    let dur = totalTime envelope
        start = startOffset + getCurrentTime DefaultContext
        source' = source
                |> startAudioBufferNode start bufferOffset (Just dur)
                |> stopAudioBufferNode (start + dur + 0.1)
        localGain' = localGain
                   |> tapNode .gain (linearRampToValue amp (start + envelope.attack))
                   |> tapNode .gain (linearRampToValue 0.0 (start + dur))
    in localGain

-- makeGrain : AudioBuffer -> GrainParams -> AudioNode a
makeGrain buffer {transposition, pan, amp, bufferOffset, startOffset, envelope} =
    let localGain = makeGain 0.0
        panner = makePanner pan
        dur = totalTime envelope
        source = createAudioBufferSourceNode DefaultContext
               |> setAudioBufferForNode buffer
               |> tapNode .playbackRate (mulParam transposition)
               |> connectNodes localGain 0 0
        _ = maybeConnect localGain panner defaultGainNode
    in startSourceAndEnv source localGain envelope bufferOffset startOffset amp

getOffset state =
    let (x, y) = state.mousePos
        (w, h) = state.dims
        ratio = toFloat x / toFloat w
        bufLen = Maybe.map getBufferDuration state.buffer 
    in ratio * withDefault 0.0 bufLen


makeParams : State -> Int -> List Float -> GrainParams
makeParams state n [x, y, z, w] =
    let baseOffset = getOffset state
        (_, mouseY) = state.mousePos
        (_, h) = state.dims
        mouseScaledY = 1.0 - (toFloat mouseY / toFloat h)
        pan = if w < 0.5 then Pan ((z * 2.0) - 1.0) else NoPan
        bufferOffset = baseOffset + (state.bufferSpread * x)
    in { defaultGrainParams | bufferOffset <- bufferOffset,
                              startOffset <- y * state.triggerSpread,
                              pan <- pan,
                              amp <- 0.7 * mouseScaledY,
                              envelope <- state.envelope }

randomParams n = list n (list 4 (float 0 1))

triggerGrains : Int -> State -> Task x ()
triggerGrains n state =
    case state.buffer of
      Just buffer' ->
           let (offsets, seed') = generate (randomParams n) state.seed
               grainsWith = makeGrain buffer' << makeParams state n
               _ = List.map grainsWith offsets
           in Signal.send actions.address (NewSeed seed')
      Nothing -> Task.succeed ()

view : State -> Element
view state =
    case state.buffer of
      Just _ -> show "Buffer loaded; mouse left/right = position in buffer, up/down = volume"
      Nothing -> show "Buffer not loaded"

-- Updates
update : Action -> State -> State
update action state =
    case action of
      NewSeed s -> { state | seed <- s }
      NewEnv e -> { state | envelope <- e }
      BufferLoaded buf -> { state | buffer <- Just buf }
      NoOp -> state

mkState state mousePos winDims = 
    { state | mousePos <- mousePos, dims <- winDims }

doAudio state =
    triggerGrains 8 state

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

-- Signals

-- guitar sample from Sub-d: https://www.freesound.org/people/Sub-d/sounds/49656/
port audioBuffer : Task err ()
port audioBuffer =
    loadAudioBufferFromUrl DefaultContext "guitar.mp3" `Task.andThen`
    \x -> Signal.send actions.address (BufferLoaded x)

controlState : Signal State
controlState = Signal.foldp update defaultState actions.signal

grainTrigger : Signal Time
grainTrigger = every (100 * millisecond)

state : Signal State
state = mkState <~ controlState 
        ~ Mouse.position
        ~ Window.dimensions

port audioState : Signal (Task x ())
port audioState = Signal.filter (isJust << .buffer) defaultState state
           |> Signal.sampleOn grainTrigger
           |> Signal.map doAudio

-- Main

main = Signal.map view state