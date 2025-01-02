module Main where

import Prelude

import CS150241Project.GameEngine (startNetworkGame)
import CS150241Project.Graphics (clearCanvas, drawImageScaled, drawRect, drawText)
import CS150241Project.Networking (Message)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Graphics.Canvas as Canvas
import Data.Array ((..), (!!))
import Data.Foldable (foldl)

width :: Number
width = 600.0

height :: Number
height = 600.0

columns :: Int
columns = 8

rows :: Int
rows = 8

tileWidth :: Number
tileWidth = Number.floor $ width / (Int.toNumber columns)

tileHeight :: Number
tileHeight = Number.floor $ height / (Int.toNumber rows)

fps :: Int
fps = 60

type GameState =
  { tickCount :: Int
  , x :: Number
  , y :: Number
  , lastReceivedMessage :: Maybe Message
  }

initialState :: Effect GameState
initialState = do
  x <- randomRange 0.0 width
  y <- randomRange 0.0 height
  pure { tickCount: 0, x, y, lastReceivedMessage: Nothing }

onTick :: (String -> Effect Unit) -> GameState -> Effect GameState
onTick send gameState = do
  log $ "Tick: " <> show gameState.tickCount

  if gameState.tickCount `mod` fps == 0 then do
    x <- randomRange 0.0 width
    y <- randomRange 0.0 height
    send $ "Moved to (" <> show x <> ", " <> show y <> ")"
    pure $ gameState { x = x, y = y, tickCount = gameState.tickCount + 1 }
  else
    pure $ gameState { tickCount = gameState.tickCount + 1 }

onMouseDown :: (String -> Effect Unit) -> { x :: Int, y :: Int } -> GameState -> Effect GameState
onMouseDown send { x, y } gameState = do
  send $ show x <> "," <> show y
  pure gameState

onKeyDown :: (String -> Effect Unit) -> String -> GameState -> Effect GameState
onKeyDown send key gameState = do
  send $ "I pressed " <> key
  pure gameState

onKeyUp :: (String -> Effect Unit) -> String -> GameState -> Effect GameState
onKeyUp _ _ gameState = pure gameState

onMessage :: (String -> Effect Unit) -> Message -> GameState -> Effect GameState
onMessage _ message gameState = do
  log $ "Received message: " <> show message
  pure $ gameState { lastReceivedMessage = Just message }

onRender :: Map.Map String Canvas.CanvasImageSource -> Canvas.Context2D -> GameState -> Effect Unit
onRender images ctx gameState = do
  renderGame

  let
    x = width / 2.0
    y = width / 2.0
    color = "white"
    font = "arial"
    size = 18

  case gameState.lastReceivedMessage of
    Nothing -> drawText ctx { x, y, color, font, size, text: "No messages received yet" }
    Just message -> drawText ctx { x, y, color, font, size, text: "Last received message: " <> message.payload }

  case Map.lookup "assets/lui_sword.jpg" images of
    Nothing -> pure unit
    Just img -> drawImageScaled ctx img { x: gameState.x, y: gameState.y, width: 100.0, height: 100.0 }

  where
  renderGame :: Effect Unit
  renderGame = do
    clearCanvas ctx { color: "black", width, height }
    renderGrid

  renderTile :: Int -> Int -> Effect Unit
  renderTile r c =
    let
      color =
        if mod (r + c) 2 == 0 then
          "white"
        else
          "black"
    in
      drawRect ctx
        { x: (Int.toNumber c) * tileWidth
        , y: (Int.toNumber r) * tileHeight
        , width: tileWidth
        , height: tileHeight
        , color
        }

  renderGrid :: Effect Unit
  renderGrid =
    let
      rs = (0 .. rows)
      cs = (0 .. columns)
    in
      -- This is a rather strange implementation, 
      -- but this was honestly the best way I could think of writing this so far
      foldl (<>) (pure unit)
        ( map
            ( \r ->
                foldl (<>) (pure unit) $ map (\c -> renderTile r c) cs
            )
            rs
        )

main :: Effect Unit
main =
  startNetworkGame
    { initialState
    , onTick
    , onMouseDown
    , onKeyDown
    , onKeyUp
    , onRender
    , onMessage
    , fps
    , width
    , height
    , ipAddress: "localhost"
    , port: 15000
    , imagePaths: [ "assets/lui_sword.jpg" ]
    }
