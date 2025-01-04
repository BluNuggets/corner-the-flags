module Main where

import Prelude

import CS150241Project.GameEngine (startNetworkGame)
import CS150241Project.Graphics (clearCanvas, drawImageScaled, drawRect, drawRectOutline, drawText)
import CS150241Project.Networking (Message)
import Data.Array (updateAt, deleteAt, (!!), (..), elem, find, filter, slice, zipWith)
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas as Canvas

boardWidth :: Number
boardWidth = 600.0

boardHeight :: Number
boardHeight = 600.0

capturedPanelWidth :: Number
capturedPanelWidth = 150.0

capturedPanelHeight :: Number
capturedPanelHeight = boardHeight

width :: Number
width = boardWidth + capturedPanelWidth

height :: Number
height = max boardHeight capturedPanelHeight

capturedPieceGap :: Number
capturedPieceGap = 10.0

maxCapturedPerPage :: Int
maxCapturedPerPage = 4

cols :: Int
cols = 8

rows :: Int
rows = 8

tileWidth :: Number
tileWidth = Number.floor $ boardWidth / (toNumber cols)

tileHeight :: Number
tileHeight = Number.floor $ boardHeight / (toNumber rows)

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

type PieceInfo =
  { pieceKind :: PieceKind
  , movements :: Array Location
  , isProtected :: Boolean
  }

type Piece =
  { info :: PieceInfo
  , player :: Int
  , location :: Location
  }

type CapturedPiece =
  { info :: PieceInfo
  , player :: Int
  }

pawnInfo :: Int -> PieceInfo
pawnInfo player =
  let
    movements =
      if player == 1 then
        [ newLocation (-1) 0 ]
      else
        [ newLocation 1 0 ]
  in
    { pieceKind: Pawn
    , movements
    , isProtected: false
    }

kingInfo :: PieceInfo
kingInfo =
  let
    drs = ((-1) .. 1)
    dcs = ((-1) .. 1)

    movements =
      map (\dr -> map (\dc -> newLocation dr dc) dcs) drs
        # foldl (<>) []
        # filter (_ /= newLocation 0 0)
  in
    { pieceKind: King
    , movements
    , isProtected: true
    }

type GameState =
  { tickCount :: Int
  , pieces :: Array Piece
  , capturedPieces :: Array CapturedPiece
  , capturedPiecesPage :: Int
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
      { info: pawnInfo player
      , player
      , location
      }

    createKing :: Int -> Location -> Piece
    createKing player location =
      { info: kingInfo
      , player
      , location
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
    , createKing 1 (newLocation 7 7)
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
    , createKing 2 (newLocation 0 0)
    ]
  pure
    { tickCount: 0
    , pieces
    , capturedPieces: []
    , capturedPiecesPage: 0
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

  let
    onMovement :: Maybe (GameState)
    onMovement = do
      index <- gameState.activePieceIndex
      piece <- gameState.pieces !! index

      let
        locationInBounds :: Location -> Boolean
        locationInBounds location =
          elem location.row (0 .. rows) && elem location.col (0 .. cols)
        possibleMovements = getAllMovements piece.info.movements piece.location # (filter locationInBounds)

      if piece.location /= clickLocation && elem clickLocation possibleMovements then do
        piecesAfterMove <- updateAt index (piece { location = clickLocation }) gameState.pieces
        case getPieceAtLocation gameState.pieces clickLocation of
          Just destPiece -> do
            if not (piece.info.isProtected || destPiece.info.isProtected || destPiece.player == gameState.currentPlayer) then do
              capturedIndex <- getPieceIndex gameState.pieces destPiece
              piecesAfterCapture <- deleteAt capturedIndex piecesAfterMove

              pure $ gameState
                { pieces = piecesAfterCapture
                , capturedPieces = gameState.capturedPieces <> [ { info: destPiece.info, player: gameState.currentPlayer } ]
                , activePieceIndex = Nothing
                }
            else pure gameState
          Nothing -> pure $ gameState { pieces = piecesAfterMove, activePieceIndex = Nothing }
      else pure gameState

  case onMovement of
    Just newGameState -> pure newGameState
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
    renderCapturedPanel gameState.capturedPieces gameState.player gameState.capturedPiecesPage

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

      lookupResult = case piece.info.pieceKind of
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
  renderActivePiece pieces mIndex =
    case mIndex of
      Just index ->
        case pieces !! index of
          Just activePiece ->
            let
              x = tileWidth * (toNumber activePiece.location.col)
              y = tileHeight * (toNumber activePiece.location.row)
            in
              drawRectOutline ctx { x, y, width: tileWidth, height: tileHeight, color: "white" }
          Nothing -> pure unit
      Nothing -> pure unit

  renderCapturedPiece :: Number -> Number -> CapturedPiece -> Effect Unit
  renderCapturedPiece x y capturedPiece =
    let
      lookupResult = case capturedPiece.info.pieceKind of
        King -> Map.lookup "assets/lui_wink_ed.jpg" images
        Lance -> Map.lookup "assets/lui_bright.jpg" images
        Pawn -> Map.lookup "assets/lui_sword.jpg" images
    in
      case lookupResult of
        Nothing -> pure unit
        Just img -> drawImageScaled ctx img { x, y, width: tileWidth, height: tileHeight }

  renderCapturedPieces :: Array CapturedPiece -> Int -> Int -> Effect Unit
  renderCapturedPieces capturedPieces player page =
    let
      playerCapturedPieces = filter (\cp -> cp.player == player) capturedPieces
      boxWidth = tileWidth
      boxHeight = (tileHeight * (toNumber maxCapturedPerPage)) + (capturedPieceGap * (toNumber $ maxCapturedPerPage - 1))
      boxX = boardWidth + (capturedPanelWidth / 2.0) - (boxWidth / 2.0)
      boxY = (capturedPanelHeight / 2.0) - (boxHeight / 2.0)

      pageStart = page * maxCapturedPerPage
      pageEnd = pageStart + maxCapturedPerPage
      playerCapturedPiecesPage = slice pageStart pageEnd playerCapturedPieces

      ys = map ((+) boxY) $ map ((*) (tileHeight + capturedPieceGap)) (map toNumber (0 .. (maxCapturedPerPage - 1)))
    in
      foldl (<>) (pure unit) $ zipWith (renderCapturedPiece boxX) ys playerCapturedPiecesPage

  renderCapturedPanel :: Array CapturedPiece -> Int -> Int -> Effect Unit
  renderCapturedPanel capturedPieces player page = do
    drawRect ctx { x: boardWidth, y: 0.0, width: capturedPanelWidth, height: capturedPanelHeight, color: "orange" }
    renderCapturedPieces capturedPieces player page

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
