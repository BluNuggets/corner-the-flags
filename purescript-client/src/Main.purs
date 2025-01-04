module Main where

import Prelude

import CS150241Project.GameEngine (startNetworkGame)
import CS150241Project.Graphics (clearCanvas, drawImageScaled, drawRect, drawRectOutline, drawText)
import CS150241Project.Networking (Message, PlayerId(..))
import Data.Array (deleteAt, elem, filter, find, findIndex, length, slice, updateAt, zipWith, (!!), (..))
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

-- Helper Funtions

-- This can't be a type-class due to the "Orphan Instance" error
-- While I could have written a newtype, I just didn't feel it was worth the mess that comes with it
isSamePlayer :: PlayerId -> PlayerId -> Boolean
isSamePlayer a b =
  case a, b of
    Player1, Player1 -> true
    Player2, Player2 -> true
    _, _ -> false

mirror :: forall a. Ring a => a -> a -> a -> a
mirror bottom top x = top - (x - bottom)

-- Captured Panel UI Definitions
type CapturedPanel =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , color :: String
  , slotGap :: Number
  , capturedPieceSlots :: Array CapturedPieceSlot
  , buttons :: Array Button
  , pageText :: Maybe Text
  , maxCapturedPerPage :: Int
  , currentPage :: Int
  , currentPageCount :: Int
  }

type CapturedPieceSlot =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

type Text =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , text :: String
  , textColor :: String
  , font :: String
  , fontSize :: Int
  }

-- Note: Button onClick handling is weird due to compiler limitations on circular/self-referential typing
data ButtonActions
  = PreviousPage
  | NextPage

type Button =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , text :: String
  , textColor :: String
  , font :: String
  , fontSize :: Int
  , onClickAction :: ButtonActions
  }

initializeCapturedPanel :: CapturedPanel
initializeCapturedPanel =
  let
    capturedPanel :: CapturedPanel
    capturedPanel =
      { x: boardWidth
      , y: 0.0
      , width: capturedPanelWidth
      , height: capturedPanelHeight
      , color: "orange"
      , slotGap: 10.0
      , capturedPieceSlots: []
      , buttons: []
      , pageText: Nothing
      , maxCapturedPerPage: 4
      , currentPage: 0
      , currentPageCount: 1
      }

    capturedPieceSlots :: Array CapturedPieceSlot
    capturedPieceSlots =
      let
        boxWidth = tileWidth
        boxHeight = (tileHeight * (toNumber capturedPanel.maxCapturedPerPage))
          + (capturedPanel.slotGap * (toNumber $ capturedPanel.maxCapturedPerPage - 1))
        boxX = boardWidth + (capturedPanel.width / 2.0) - (boxWidth / 2.0)
        boxY = (capturedPanel.height / 2.0) - (boxHeight / 2.0)
      in
        map
          ( \index ->
              { x: boxX
              , y: boxY + ((tileHeight + capturedPanel.slotGap) * (toNumber index))
              , width: tileWidth
              , height: tileHeight
              }
          )
          (0 .. (capturedPanel.maxCapturedPerPage - 1))

    capturedPanelPageText :: Maybe Text
    capturedPanelPageText =
      let
        textWidth = capturedPanel.width
        fontSize = 14
        textHeight = toNumber fontSize
      in
        Just
          { x: capturedPanel.x
          , y: 580.0
          , width: textWidth
          , height: textHeight
          , text: "Page 1 of 1"
          , textColor: "black"
          , font: "arial"
          , fontSize
          }

    capturedPanelButtons :: Array Button
    capturedPanelButtons =
      let
        buttonWidth = capturedPanel.width / 2.0
        fontSize = 14
        buttonHeight = 2.0 * toNumber fontSize
      in
        [ { x: capturedPanel.x
          , y: 550.0
          , width: buttonWidth
          , height: buttonHeight
          , text: "<<< Prev"
          , textColor: "black"
          , font: "arial"
          , fontSize
          , onClickAction: PreviousPage
          }
        , { x: capturedPanel.x + buttonWidth
          , y: 550.0
          , width: buttonWidth
          , height: buttonHeight
          , text: "Next >>>"
          , textColor: "black"
          , font: "arial"
          , fontSize
          , onClickAction: NextPage
          }
        ]
  in
    capturedPanel { capturedPieceSlots = capturedPieceSlots, pageText = capturedPanelPageText, buttons = capturedPanelButtons }

-- Board Definitions
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

mirrorLocation :: Location -> Location
mirrorLocation location =
  { row: mirror 0 (rows - 1) location.row
  , col: mirror 0 (cols - 1) location.col
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
  , player :: PlayerId
  , location :: Location
  }

type CapturedPiece =
  { info :: PieceInfo
  , player :: PlayerId
  }

pawnInfo :: PlayerId -> PieceInfo
pawnInfo player =
  let
    movements = case player of
      Player1 -> [ newLocation (-1) 0 ]
      Player2 -> [ newLocation 1 0 ]
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
  , player :: PlayerId
  , currentPlayer :: PlayerId
  , activePieceIndex :: Maybe Int
  , activeCapturedPieceIndex :: Maybe Int
  , capturedPanel :: CapturedPanel
  , lastReceivedMessage :: Maybe Message
  , debugString :: String
  }

initialState :: Effect GameState
initialState = do
  let
    createPawn :: PlayerId -> Location -> Piece
    createPawn player location =
      { info: pawnInfo player
      , player
      , location
      }

    createKing :: PlayerId -> Location -> Piece
    createKing player location =
      { info: kingInfo
      , player
      , location
      }

  pieces <- pure $
    [
      -- Player 1
      createPawn Player1 (newLocation 6 0)
    , createPawn Player1 (newLocation 6 1)
    , createPawn Player1 (newLocation 6 2)
    , createPawn Player1 (newLocation 6 3)
    , createPawn Player1 (newLocation 6 4)
    , createPawn Player1 (newLocation 6 5)
    , createPawn Player1 (newLocation 6 6)
    , createPawn Player1 (newLocation 6 7)
    , createKing Player1 (newLocation 7 0)
    ,
      -- Player 2
      createPawn Player2 (newLocation 1 0)
    , createPawn Player2 (newLocation 1 1)
    , createPawn Player2 (newLocation 1 2)
    , createPawn Player2 (newLocation 1 3)
    , createPawn Player2 (newLocation 1 4)
    , createPawn Player2 (newLocation 1 5)
    , createPawn Player2 (newLocation 1 6)
    , createPawn Player2 (newLocation 1 7)
    , createKing Player2 (newLocation 0 0)
    ]

  pure
    { tickCount: 0
    , pieces
    , capturedPieces: []
    , player: Player1
    , currentPlayer: Player1
    , activePieceIndex: Nothing
    , activeCapturedPieceIndex: Nothing
    , capturedPanel: initializeCapturedPanel
    , lastReceivedMessage: Nothing
    , debugString: ""
    }

onTick :: (String -> Effect Unit) -> GameState -> Effect GameState
onTick _ gameState = do
  log $ show $ gameState.activeCapturedPieceIndex
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
onMouseDown _ { x, y } gameState =
  if isSamePlayer gameState.player gameState.currentPlayer then
    (pure gameState)
      <#> checkClickCapturedPanel
      <#> checkClickBoard
  else
    (pure gameState)
      <#> checkClickCapturedPanel

  where
  nx = toNumber x
  ny = toNumber y

  checkClickButton :: GameState -> Button -> GameState
  checkClickButton state button' =
    if isClickingButton button' then
      case button'.onClickAction of
        PreviousPage ->
          let
            newPage = state.capturedPanel.currentPage - 1
            clampedNewPage = clamp 0 (state.capturedPanel.currentPageCount - 1) newPage
          in
            state { capturedPanel { currentPage = clampedNewPage } }
        NextPage ->
          let
            newPage = state.capturedPanel.currentPage + 1
            clampedNewPage = clamp 0 (state.capturedPanel.currentPageCount - 1) newPage
          in
            state { capturedPanel { currentPage = clampedNewPage } }
    else
      state
    where
    isClickingButton :: Button -> Boolean
    isClickingButton button =
      buttonLeft <= nx
        && nx <= buttonRight
        && buttonTop <= ny
        && ny <= buttonBottom
      where
      buttonLeft = button.x
      buttonRight = button.x + button.width
      buttonTop = button.y
      buttonBottom = button.y + button.height

  checkClickCapturedPieces :: GameState -> GameState
  checkClickCapturedPieces state =
    let
      isClickingCapturedPiece :: CapturedPieceSlot -> Boolean
      isClickingCapturedPiece slot =
        slotLeft <= nx
          && nx <= slotRight
          && slotTop <= ny
          && ny <= slotBottom
        where
        slotLeft = slot.x
        slotRight = slot.x + slot.width
        slotTop = slot.y
        slotBottom = slot.y + slot.height

      pageOffset = state.capturedPanel.currentPage * state.capturedPanel.maxCapturedPerPage

      mIndex =
        map isClickingCapturedPiece state.capturedPanel.capturedPieceSlots
          # findIndex (_ == true)
    in
      case mIndex of
        Just index ->
          case state.capturedPieces !! (pageOffset + index) of
            Just _ -> state { activeCapturedPieceIndex = Just (pageOffset + index), activePieceIndex = Nothing }
            Nothing -> state
        Nothing -> state

  checkClickCapturedPanel :: GameState -> GameState
  checkClickCapturedPanel state =
    let
      buttons = state.capturedPanel.buttons
    in
      foldl checkClickButton state buttons
        # checkClickCapturedPieces

  placeCapturedPiece :: Location -> Int -> GameState -> GameState
  placeCapturedPiece clickLocation index state =
    state

  movePiece :: Location -> Int -> GameState -> GameState
  movePiece clickLocation index state =
    let
      locationInBounds :: Location -> Boolean
      locationInBounds location =
        elem location.row (0 .. (rows - 1)) && elem location.col (0 .. (cols - 1))

      mNewState = do
        piece <- state.pieces !! index
        possibleMovements <- pure $ getAllMovements piece.info.movements piece.location # (filter locationInBounds)

        if piece.location /= clickLocation && elem clickLocation possibleMovements then do
          piecesAfterMove <- updateAt index (piece { location = clickLocation }) state.pieces
          case getPieceAtLocation state.pieces clickLocation of
            Just destPiece -> do
              if not (piece.info.isProtected || destPiece.info.isProtected || isSamePlayer destPiece.player state.currentPlayer) then do
                capturedIndex <- getPieceIndex state.pieces destPiece
                piecesAfterCapture <- deleteAt capturedIndex piecesAfterMove

                let
                  newCapturedPieces = state.capturedPieces <> [ { info: destPiece.info, player: state.currentPlayer } ]
                  newCount = length newCapturedPieces
                  newPageCount = 1 + (newCount - 1) / state.capturedPanel.maxCapturedPerPage

                pure $ state
                  { pieces = piecesAfterCapture
                  , capturedPieces = state.capturedPieces <> [ { info: destPiece.info, player: state.currentPlayer } ]
                  , activePieceIndex = Nothing
                  , capturedPanel { currentPageCount = newPageCount }
                  }
              else pure $ state { activePieceIndex = Nothing }
            Nothing -> pure $ state { pieces = piecesAfterMove, activePieceIndex = Nothing }
        else pure $ state { activePieceIndex = Nothing }
    in
      case mNewState of
        Just newState -> newState
        Nothing -> state

  selectPiece :: Location -> GameState -> GameState
  selectPiece clickLocation state =
    let
      foundPiece = getPieceAtLocation state.pieces clickLocation
    in
      case foundPiece of
        Just piece ->
          if isSamePlayer piece.player state.player then
            case getPieceIndex state.pieces piece of
              Just index -> state { activePieceIndex = Just index, activeCapturedPieceIndex = Nothing }
              Nothing -> state
          else
            state
        Nothing -> state

  checkClickBoard :: GameState -> GameState
  checkClickBoard state =
    let
      clickLocation = case state.player of
        Player1 -> posToLocation y x
        Player2 -> mirrorLocation $ posToLocation y x
    in
      case state.activePieceIndex, state.activeCapturedPieceIndex of
        Just index, Nothing -> movePiece clickLocation index state
        Nothing, Just index -> placeCapturedPiece clickLocation index state
        _, _ -> selectPiece clickLocation state

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
    renderBoard gameState.player gameState.pieces gameState.activePieceIndex
    renderCapturedPanel gameState.capturedPanel gameState.capturedPieces gameState.activeCapturedPieceIndex

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
      cs = (0 .. (cols - 1))
    in
      foldl (<>) (pure unit) $ (renderTile row) <$> cs

  renderGrid :: Effect Unit
  renderGrid =
    let
      rs = (0 .. (rows - 1))
    in
      foldl (<>) (pure unit) $ renderRow <$> rs

  renderPiece :: PlayerId -> Piece -> Effect Unit
  renderPiece player piece =
    let
      location = case player of
        Player1 -> piece.location
        Player2 -> mirrorLocation piece.location
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

  renderPieces :: PlayerId -> Array Piece -> Effect Unit
  renderPieces player pieces =
    foldl (<>) (pure unit) $ (renderPiece player) <$> pieces

  renderActivePiece :: PlayerId -> Array Piece -> Maybe Int -> Effect Unit
  renderActivePiece player pieces mIndex =
    case mIndex of
      Just index ->
        case pieces !! index of
          Just activePiece ->
            let
              location = case player of
                Player1 -> activePiece.location
                Player2 -> mirrorLocation activePiece.location
              x = tileHeight * (toNumber location.col)
              y = tileHeight * (toNumber location.row)
            in
              drawRectOutline ctx { x, y, width: tileWidth, height: tileHeight, color: "white" }
          Nothing -> pure unit
      Nothing -> pure unit

  renderBoard :: PlayerId -> Array Piece -> Maybe Int -> Effect Unit
  renderBoard player pieces mIndex = do
    renderGrid
    renderPieces player pieces
    renderActivePiece player pieces mIndex

  renderCapturedPiece :: CapturedPieceSlot -> CapturedPiece -> Effect Unit
  renderCapturedPiece slot capturedPiece =
    let
      lookupResult = case capturedPiece.info.pieceKind of
        King -> Map.lookup "assets/lui_wink_ed.jpg" images
        Lance -> Map.lookup "assets/lui_bright.jpg" images
        Pawn -> Map.lookup "assets/lui_sword.jpg" images
    in
      case lookupResult of
        Nothing -> pure unit
        Just img -> drawImageScaled ctx img { x: slot.x, y: slot.y, width: slot.width, height: slot.height }

  renderCapturedPieces :: Array CapturedPieceSlot -> Array CapturedPiece -> Int -> Effect Unit
  renderCapturedPieces capturedPieceSlots capturedPieces page =
    let
      pageStart = page * maxCapturedPerPage
      pageEnd = pageStart + maxCapturedPerPage
      pagedCapturedPieces = slice pageStart pageEnd capturedPieces
    in
      foldl (<>) (pure unit) $ zipWith renderCapturedPiece capturedPieceSlots pagedCapturedPieces

  renderActiveCapturedPiece :: Array CapturedPieceSlot -> Maybe Int -> Int -> Int -> Effect Unit
  renderActiveCapturedPiece slots mIndex page maxPerPage =
    case mIndex of
      Just index ->
        let
          pageIndex = index / maxPerPage
          slotIndex = mod index maxPerPage
        in
          if page == pageIndex then
            case slots !! slotIndex of
              Just slot ->
                drawRectOutline ctx { x: slot.x, y: slot.y, width: slot.width, height: slot.height, color: "white" }
              Nothing -> pure unit
          else pure unit
      Nothing -> pure unit

  renderCapturedPanelButton :: Button -> Effect Unit
  renderCapturedPanelButton panelButton = do
    drawRect ctx
      { x: panelButton.x
      , y: panelButton.y
      , width: panelButton.width
      , height: panelButton.height
      , color: "red"
      }

    drawText ctx
      { x: panelButton.x + panelButton.width / 2.0
      , y: panelButton.y + panelButton.height / 2.0 + (toNumber panelButton.fontSize) / 2.0
      , text: panelButton.text
      , color: panelButton.textColor
      , font: panelButton.font
      , size: panelButton.fontSize
      }

  renderCapturedPanelButtons :: Array Button -> Effect Unit
  renderCapturedPanelButtons panelButtons =
    foldl (<>) (pure unit) $ renderCapturedPanelButton <$> panelButtons

  renderPageText :: Maybe Text -> Int -> Int -> Effect Unit
  renderPageText mPageText page pageCount =
    case mPageText of
      Just pageText ->
        drawText ctx
          { x: pageText.x + pageText.width / 2.0
          , y: pageText.y + pageText.height / 2.0 + (toNumber pageText.fontSize) / 2.0
          , text: "Page " <> show (page + 1) <> " of " <> show pageCount
          , color: pageText.textColor
          , font: pageText.font
          , size: pageText.fontSize
          }
      Nothing -> pure unit

  renderCapturedPanel :: CapturedPanel -> Array CapturedPiece -> Maybe Int -> Effect Unit
  renderCapturedPanel capturedPanel capturedPieces activeCapturedPieceIndex = do
    drawRect ctx { x: capturedPanel.x, y: capturedPanel.y, width: capturedPanel.width, height: capturedPanel.height, color: capturedPanel.color }
    renderPageText capturedPanel.pageText capturedPanel.currentPage capturedPanel.currentPageCount
    renderCapturedPanelButtons capturedPanel.buttons
    renderCapturedPieces capturedPanel.capturedPieceSlots capturedPieces capturedPanel.currentPage
    renderActiveCapturedPiece capturedPanel.capturedPieceSlots activeCapturedPieceIndex capturedPanel.currentPage capturedPanel.maxCapturedPerPage

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
        , "assets/lui_bright.jpg"
        ]
    }
