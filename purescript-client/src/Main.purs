module Main where

import Prelude

import CS150241Project.GameEngine (startNetworkGame)
import CS150241Project.Graphics (clearCanvas, drawImageScaled, drawRect, drawText)
import CS150241Project.Networking (Message)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Graphics.Canvas as Canvas
import Data.Array ((..))
import Data.Foldable (foldl)

width :: Number
width = 600.0

height :: Number
height = 600.0

cols :: Int
cols = 8

rows :: Int
rows = 8

tileWidth :: Number
tileWidth = Number.floor $ width / (toNumber cols)

tileHeight :: Number
tileHeight = Number.floor $ height / (toNumber rows)

fps :: Int
fps = 60

data PieceKind
  = King
  | Lance
  | Pawn

type Location =
  { row :: Int
  , col :: Int
  }

type Piece =
  { player :: Int
  , pieceKind :: PieceKind
  , location :: Location
  , movement :: Array Location
  , isProtected :: Boolean
  , canCapture :: Boolean
  }

type GameState =
  { tickCount :: Int
  , pieces :: Array Piece
  , x :: Number
  , y :: Number
  , lastReceivedMessage :: Maybe Message
  }

initialState :: Effect GameState
initialState = do
  x <- randomRange 0.0 width
  y <- randomRange 0.0 height

  let
    createPawn :: Int -> Location -> Piece
    createPawn player location =
      let
        movement =
          if player == 1 then
            [ { row: 1, col: 0 } ]
          else
            [ { row: -1, col: 0 } ]
      in
        { player
        , pieceKind: Pawn
        , location
        , movement
        , isProtected: false
        , canCapture: false
        }

  pieces <- pure $
    [
      -- Player 1
      createPawn 1 { row: 1, col: 0 }
    -- Player 2
    ]
  pure { tickCount: 0, pieces, x, y, lastReceivedMessage: Nothing }

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
    renderPieces gameState.pieces

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
        { x: (toNumber c) * tileWidth
        , y: (toNumber r) * tileHeight
        , width: tileWidth
        , height: tileHeight
        , color
        }

  renderRow :: Int -> Effect Unit
  renderRow row =
    let
      cs = (0 .. cols)
    in
      foldl (<>) (pure unit) $ (renderTile row) <$> cs

  renderGrid :: Effect Unit
  renderGrid =
    let
      rs = (0 .. rows)
    in
      foldl (<>) (pure unit) $ renderRow <$> rs

  renderPiece :: Piece -> Effect Unit
  renderPiece piece =
    let
      location = piece.location
      x = tileWidth * (toNumber location.col)
      y = tileHeight * (toNumber location.row)

      lookupResult = case piece.pieceKind of
        King -> Map.lookup "assets/lui_wink_ed.jpg" images
        Lance -> Map.lookup "assets/lui_bright.jpg" images
        Pawn -> Map.lookup "assets/lui_sword.jpg" images
    in
      case lookupResult of
        Nothing -> pure unit
        Just img -> drawImageScaled ctx img { x, y, width: tileWidth, height: tileHeight }

  renderPieces :: Array Piece -> Effect Unit
  renderPieces pieces =
    foldl (<>) (pure unit) $ renderPiece <$> pieces

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
    , imagePaths:
        [ "assets/lui_wink_ed.jpg"
        , "assets/lui_sword.jpg"
        , "assets/lui_sword.jpg"
        ]
    }
