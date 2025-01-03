module Main where

import Prelude

import CS150241Project.GameEngine (startNetworkGame)
import CS150241Project.Graphics (clearCanvas, drawImageScaled, drawRect, drawRectOutline, drawText)
import CS150241Project.Networking (Message)
import Data.Array (updateAt, (!!), (..), elem)
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas as Canvas

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

newLocation :: Int -> Int -> Location
newLocation row col =
  { row
  , col
  }

posToLocation :: Int -> Int -> Location
posToLocation y x =
  let
    row = y / (floor tileWidth)
    col = x / (floor tileHeight)
  in
    { row, col }

getPieceAtLocation :: Array Piece -> Location -> Maybe Piece
getPieceAtLocation pieces location =
  loop 0
  where
  loop :: Int -> Maybe Piece
  loop i =
    case pieces !! i of
      Just piece ->
        if piece.location == location then
          Just piece
        else
          loop (i + 1)
      Nothing -> Nothing

getPieceIndex :: Array Piece -> Piece -> Maybe Int
getPieceIndex pieces targetPiece =
  loop 0
  where
  loop :: Int -> Maybe Int
  loop i =
    case pieces !! i of
      Just piece ->
        if piece.location == targetPiece.location then
          Just i
        else
          loop (i + 1)
      Nothing -> Nothing

getAllMovements :: Array Location -> Location -> Array Location
getAllMovements deltas location =
  deltas <#> (add location)

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
  , player :: Int
  , currentPlayer :: Int
  , activePieceIndex :: Maybe Int
  , lastReceivedMessage :: Maybe Message
  }

initialState :: Effect GameState
initialState = do
  let
    createPawn :: Int -> Location -> Piece
    createPawn player location =
      let
        movement =
          if player == 1 then
            [ newLocation (-1) 0 ]
          else
            [ newLocation 1 0 ]
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
      createPawn 1 (newLocation 6 0)
    , createPawn 1 (newLocation 6 1)
    , createPawn 1 (newLocation 6 2)
    , createPawn 1 (newLocation 6 3)
    , createPawn 1 (newLocation 6 4)
    , createPawn 1 (newLocation 6 5)
    , createPawn 1 (newLocation 6 6)
    , createPawn 1 (newLocation 6 7)
    ,
      -- Player 2
      createPawn 2 (newLocation 1 0)
    , createPawn 2 (newLocation 1 1)
    , createPawn 2 (newLocation 1 2)
    , createPawn 2 (newLocation 1 3)
    , createPawn 2 (newLocation 1 4)
    , createPawn 2 (newLocation 1 5)
    , createPawn 2 (newLocation 1 6)
    , createPawn 2 (newLocation 1 7)
    ]
  pure
    { tickCount: 0
    , pieces
    , player: 1
    , currentPlayer: 1
    , activePieceIndex: Nothing
    , lastReceivedMessage: Nothing
    }

onTick :: (String -> Effect Unit) -> GameState -> Effect GameState
onTick send gameState = do
  -- log $ "Tick: " <> show gameState.tickCount

  {--
if gameState.tickCount `mod` fps == 0 then do
  y <- randomRange 0.0 height
  send $ "Moved to (" <> show x <> ", " <> show y <> ")"
  pure $ gameState { x = x, y = y, tickCount = gameState.tickCount + 1 }
else
--}
  pure $ gameState { tickCount = gameState.tickCount + 1 }

onMouseDown :: (String -> Effect Unit) -> { x :: Int, y :: Int } -> GameState -> Effect GameState
onMouseDown send { x, y } gameState = do
  send $ show x <> "," <> show y
  log $ show y <> "," <> show x

  clickLocation <- pure $ posToLocation y x

  case gameState.activePieceIndex of
    Just index ->
      case gameState.pieces !! index of
        Just piece ->
          let
            possibleMovements = getAllMovements piece.movement piece.location
          in
            if piece.location /= clickLocation && elem clickLocation possibleMovements then
              case updateAt index (piece { location = clickLocation }) gameState.pieces of
                Just newPieces -> pure $ gameState { pieces = newPieces, activePieceIndex = Nothing }
                Nothing -> pure gameState
            else
              pure $ gameState { activePieceIndex = Nothing }
        Nothing -> pure gameState
    Nothing -> do
      foundPiece <- pure $ getPieceAtLocation gameState.pieces clickLocation
      case foundPiece of
        Just piece ->
          if piece.player == gameState.player then
            case getPieceIndex gameState.pieces piece of
              Just index -> pure $ gameState { activePieceIndex = Just index }
              Nothing -> pure gameState
          else
            pure gameState
        Nothing -> pure gameState

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

  where
  renderGame :: Effect Unit
  renderGame = do
    clearCanvas ctx { color: "black", width, height }
    renderGrid
    renderPieces gameState.pieces
    renderActivePiece gameState.pieces gameState.activePieceIndex

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

  renderActivePiece :: Array Piece -> Maybe Int -> Effect Unit
  renderActivePiece pieces mIndex = do
    _ <- pure do
      index <- mIndex
      activePiece <- pieces !! index

      x <- pure $ tileWidth * (toNumber activePiece.location.col)
      y <- pure $ tileHeight * (toNumber activePiece.location.row)
      pure $ drawRectOutline ctx { x, y, width: tileWidth, height: tileHeight, color: "orange" }

    pure unit

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
