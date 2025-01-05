module Main where

import Prelude

import CS150241Project.GameEngine (startNetworkGame)
import CS150241Project.Graphics (clearCanvas, drawImageScaled, drawRect, drawRectOutline, drawText)
import CS150241Project.Networking (Message, PlayerId(..))
import Data.Array (all, deleteAt, elem, filter, find, findIndex, length, slice, updateAt, zipWith, (!!), (..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas as Canvas
import Simple.JSON as JSON

capturedPanelWidth :: Number
capturedPanelWidth = 150.0

capturedPieceGap :: Number
capturedPieceGap = 10.0

maxCapturedPerPage :: Int
maxCapturedPerPage = 4

cols :: Int
cols = 8

rows :: Int
rows = 8

--might replace tileWidth and tileHeight with tileLength or tileSide 
-- Currently, this means that each side of the board is AT LEAST 600.00
tileLength :: Number
tileLength = Number.floor $ 600.00 / (toNumber $ max cols rows)

tileWidth :: Number
tileWidth = Number.floor $ 600.00 / (toNumber $ max cols rows)

tileHeight :: Number
tileHeight = Number.floor $ 600.00 / (toNumber $ max cols rows)

boardWidth :: Number
boardWidth = Number.floor $ tileLength * (toNumber cols)

boardHeight :: Number
boardHeight = Number.floor $ tileLength * (toNumber rows)

capturedPanelHeight :: Number
capturedPanelHeight = boardHeight

width :: Number
width = boardWidth + capturedPanelWidth

height :: Number
height = max boardHeight capturedPanelHeight

actionsPerTurn :: Int
actionsPerTurn = 3

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

getEnemyPlayer :: PlayerId -> PlayerId
getEnemyPlayer player =
  case player of
    Player1 -> Player2
    Player2 -> Player1

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

initializeCapturedPanel :: PlayerId -> CapturedPanel
initializeCapturedPanel player =
  let
    -- PlayerId is just needed for coloring the panel
    panelColor = case player of
      Player1 -> "sandybrown"
      Player2 -> "deepskyblue"

    capturedPanel :: CapturedPanel
    capturedPanel =
      { x: boardWidth
      , y: 0.0
      , width: capturedPanelWidth
      , height: capturedPanelHeight
      , color: panelColor
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
          , y: capturedPanel.height - 20.0
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
          , y: capturedPanel.height - 50.0
          , width: buttonWidth
          , height: buttonHeight
          , text: "<<< Prev"
          , textColor: "black"
          , font: "arial"
          , fontSize
          , onClickAction: PreviousPage
          }
        , { x: capturedPanel.x + buttonWidth
          , y: capturedPanel.height - 50.0
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

derive instance Generic PieceKind _

instance Show PieceKind where
  show = genericShow

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

-- Prefering this over `show` due to JSON parsing
pieceKindToString :: PieceKind -> String
pieceKindToString King = "King"
pieceKindToString Lance = "Lance"
pieceKindToString Pawn = "Pawn"

pieceKindFromString :: String -> Maybe PieceKind
pieceKindFromString "King" = Just King
pieceKindFromString "Lance" = Just Lance
pieceKindFromString "Pawn" = Just Pawn
pieceKindFromString _ = Nothing

class Pieceable k where
  getPieceInfo :: k -> PieceInfo
  createPiece :: k -> PlayerId -> Location -> Piece

instance Pieceable PieceKind where
  getPieceInfo King =
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
  getPieceInfo Lance =
    let
      movements =
        [ newLocation (-1) (-1)
        , newLocation (-1) 1
        , newLocation 1 (-1)
        , newLocation 1 1
        ]
    in
      { pieceKind: Lance
      , movements
      , isProtected: false
      }
  getPieceInfo Pawn =
    let
      movements = [ newLocation (-1) 0 ]
    in
      { pieceKind: Pawn
      , movements
      , isProtected: false
      }

  createPiece pk player location =
    { info: getPieceInfo pk
    , player
    , location
    }

type Location =
  { row :: Int
  , col :: Int
  }

newLocation :: Int -> Int -> Location
newLocation row col =
  { row
  , col
  }

-- Mirrors absolute board locations relative to board center
mirrorLocation :: Location -> Location
mirrorLocation location =
  { row: mirror 0 (rows - 1) location.row
  , col: mirror 0 (cols - 1) location.col
  }

-- Mirrors deltas relative to 0 0
mirrorDelta :: Location -> Location
mirrorDelta location =
  { row: mirror (1 - rows) (rows - 1) location.row
  , col: mirror (1 - cols) (cols - 1) location.col
  }

posToLocation :: Int -> Int -> Location
posToLocation y x =
  let
    row = y / (floor tileWidth)
    col = x / (floor tileHeight)
  in
    { row, col }

locationInBounds :: Location -> Boolean
locationInBounds location =
  elem location.row (0 .. (rows - 1)) && elem location.col (0 .. (cols - 1))

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

getAllMovements :: PlayerId -> Location -> Array Location -> Array Location
getAllMovements player location deltas =
  case player of
    Player1 -> deltas <#> (add location)
    Player2 -> deltas <#> mirrorDelta <#> (add location)

-- Game Over Definitions
data GameOverState
  = Winner PlayerId
  | Draw
  | None

-- Message Formats

type MovePayload =
  { frame :: Int
  , message_type :: String
  , message_content ::
      { move_src :: Location
      , move_dest :: Location
      }
  }

createMovePayload :: GameState -> Location -> Location -> MovePayload
createMovePayload state srcLocation destLocation =
  { frame: state.tickCount
  , message_type: "move"
  , message_content:
      { move_src: srcLocation
      , move_dest: destLocation
      }
  }

type PlacePayload =
  { frame :: Int
  , message_type :: String
  , message_content ::
      { place_piece_kind :: String
      , place_dest :: Location
      }
  }

createPlacePayload :: GameState -> PieceKind -> Location -> PlacePayload
createPlacePayload state pieceKind destLocation =
  { frame: state.tickCount
  , message_type: "place"
  , message_content:
      { place_piece_kind: pieceKindToString pieceKind
      , place_dest: destLocation
      }
  }

type PingPayload =
  { frame :: Int
  , message_type :: String
  }

createPingPayload :: GameState -> PingPayload
createPingPayload state =
  { frame: state.tickCount
  , message_type: "ping"
  }

-- Game state and logic
type GameState =
  { tickCount :: Int
  , pieces :: Array Piece
  , capturedPieces :: Array CapturedPiece
  , player :: PlayerId
  , currentPlayer :: PlayerId
  , turn :: Int
  , action :: Int
  , activePieceIndex :: Maybe Int
  , activeCapturedPieceIndex :: Maybe Int
  , capturedPanel :: CapturedPanel
  , gameOverState :: GameOverState
  , lastReceivedMessage :: Maybe Message
  , sentPing :: Boolean
  , receivedPing :: Boolean
  , debugString :: String
  }

updateTurnActions :: GameState -> GameState
updateTurnActions state =
  if state.action + 1 <= actionsPerTurn then
    state { action = state.action + 1 }
  else
    state { turn = state.turn + 1, action = 1, currentPlayer = getEnemyPlayer state.currentPlayer }

initialState :: Effect GameState
initialState = do
  pieces <- pure $
    [
      -- Player 1
      createPiece Pawn Player1 (newLocation 6 0)
    , createPiece Pawn Player1 (newLocation 6 1)
    , createPiece Pawn Player1 (newLocation 6 2)
    , createPiece Pawn Player1 (newLocation 6 3)
    , createPiece Pawn Player1 (newLocation 6 4)
    , createPiece Pawn Player1 (newLocation 6 5)
    , createPiece Pawn Player1 (newLocation 6 6)
    , createPiece Pawn Player1 (newLocation 6 7)
    , createPiece King Player1 (newLocation 7 0)
    ,
      -- Player 2
      createPiece Pawn Player2 (newLocation 1 0)
    , createPiece Pawn Player2 (newLocation 1 1)
    , createPiece Pawn Player2 (newLocation 1 2)
    , createPiece Pawn Player2 (newLocation 1 3)
    , createPiece Pawn Player2 (newLocation 1 4)
    , createPiece Pawn Player2 (newLocation 1 5)
    , createPiece Pawn Player2 (newLocation 1 6)
    , createPiece Pawn Player2 (newLocation 1 7)
    , createPiece King Player2 (newLocation 0 0)
    ]

  pure
    { tickCount: 0
    , pieces
    , capturedPieces: []
    , player: Player1
    , currentPlayer: Player1
    , turn: 1
    , action: 1
    , activePieceIndex: Nothing
    , activeCapturedPieceIndex: Nothing
    , capturedPanel: initializeCapturedPanel Player1
    , gameOverState: None
    , lastReceivedMessage: Nothing
    , sentPing: false
    , receivedPing: false
    , debugString: ""
    }

checkGameOver :: GameState -> GameState
checkGameOver state =
  let
    -- Brute force method
    protectedPieces = filter (_.info.isProtected) state.pieces
    player = state.player
    enemyPlayer = getEnemyPlayer player

    playerProtectedPieces = filter (\p -> isSamePlayer player p.player) protectedPieces
    enemyProtectedPieces = filter (\p -> isSamePlayer enemyPlayer p.player) protectedPieces

    playerProtectedMovements =
      map (\p -> getAllMovements player p.location p.info.movements) playerProtectedPieces
        # foldl (<>) []
        # filter locationInBounds
    enemyProtectedMovements =
      map (\p -> getAllMovements enemyPlayer p.location p.info.movements) enemyProtectedPieces
        # foldl (<>) []
        # filter locationInBounds

    allPieceLocations = map (_.location) state.pieces

    isPlayerWinning = all (_ == true) $ map (\loc -> elem loc allPieceLocations) enemyProtectedMovements
    isEnemyWinning = all (_ == true) $ map (\loc -> elem loc allPieceLocations) playerProtectedMovements

    gameOverState =
      case isPlayerWinning, isEnemyWinning of
        true, true -> Draw
        true, false -> Winner player
        false, true -> Winner enemyPlayer
        false, false -> None
  in
    state { gameOverState = gameOverState }

onTick :: (String -> Effect Unit) -> GameState -> Effect GameState
onTick send gameState = do
  log $ "Debug: " <> gameState.debugString

  when (not gameState.sentPing) do
    send $ JSON.writeJSON $ createPingPayload gameState

  -- log $ "Tick: " <> show gameState.tickCount

  {--
if gameState.tickCount `mod` fps == 0 then do
  y <- randomRange 0.0 height
  send $ "Moved to (" <> show x <> ", " <> show y <> ")"
  pure $ gameState { x = x, y = y, tickCount = gameState.tickCount + 1 }
else
--}
  pure $ gameState { tickCount = gameState.tickCount + 1, sentPing = true }

onMouseDown :: (String -> Effect Unit) -> { x :: Int, y :: Int } -> GameState -> Effect GameState
onMouseDown send { x, y } gameState =
  case gameState.gameOverState of
    None ->
      if isSamePlayer gameState.player gameState.currentPlayer then
        (pure gameState)
          <#> checkClickCapturedPanel
          # checkClickBoard
          <#> checkGameOver
      else
        (pure gameState)
          <#> checkClickCapturedPanel
    _ ->
      pure gameState

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
      if mIndex /= state.activeCapturedPieceIndex then
        case mIndex of
          Just index ->
            case state.capturedPieces !! (pageOffset + index) of
              Just _ -> state { activeCapturedPieceIndex = Just (pageOffset + index), activePieceIndex = Nothing }
              Nothing -> state
          Nothing -> state
      else
        state { activeCapturedPieceIndex = Nothing }

  checkClickCapturedPanel :: GameState -> GameState
  checkClickCapturedPanel state =
    let
      buttons = state.capturedPanel.buttons
    in
      foldl checkClickButton state buttons
        # checkClickCapturedPieces

  placeCapturedPiece :: Location -> Int -> Effect GameState -> Effect GameState
  placeCapturedPiece clickLocation index eState = do
    state <- eState

    let
      mResult :: Maybe { newState :: GameState, place_piece_kind :: Maybe PieceKind, place_dest :: Maybe Location }
      mResult = do
        capturedPiece <- state.capturedPieces !! index
        -- Assuming that we also cannot place a piece 
        -- that blocks the movement of our own protected piece
        protectedPieces <- pure $ filter (_.info.isProtected) state.pieces
        allPieceLocations <- pure $ map (_.location) state.pieces
        possibleProtectedMovements <-
          map (\p -> getAllMovements p.player p.location p.info.movements) protectedPieces
            # foldl (<>) []
            # pure
        invalidLocations <- pure $ allPieceLocations <> possibleProtectedMovements

        if locationInBounds clickLocation && not (elem clickLocation invalidLocations) then do
          newCapturedPieces <- deleteAt index state.capturedPieces

          let
            newPieces = state.pieces <> [ createPiece capturedPiece.info.pieceKind capturedPiece.player clickLocation ]
            newCount = length newCapturedPieces
            newPageCount = max 1 (1 + (newCount - 1) / state.capturedPanel.maxCapturedPerPage)
            newPage = min state.capturedPanel.currentPage (newPageCount - 1)

          newState <- pure $ (updateTurnActions state)
            { pieces = newPieces
            , capturedPieces = newCapturedPieces
            , activeCapturedPieceIndex = Nothing
            , capturedPanel { currentPage = newPage, currentPageCount = newPageCount }
            }

          Just
            { newState: newState
            , place_piece_kind: Just capturedPiece.info.pieceKind
            , place_dest: Just clickLocation
            }
        else if locationInBounds clickLocation then
          -- If clicking on the board but not on a placable tile, 
          -- deselect then attempt to select a new piece
          Just
            { newState: selectPiece clickLocation (state { activeCapturedPieceIndex = Nothing })
            , place_piece_kind: Nothing
            , place_dest: Nothing
            }
        else
          Just
            { newState: state
            , place_piece_kind: Nothing
            , place_dest: Nothing
            }

    case mResult of
      Just result ->
        case result.place_piece_kind, result.place_dest of
          Just kind, Just dest -> do
            send $ JSON.writeJSON $ createPlacePayload result.newState kind dest
            pure result.newState
          _, _ -> pure result.newState
      Nothing -> pure state

  movePiece :: Location -> Int -> Effect GameState -> Effect GameState
  movePiece clickLocation index eState = do
    state <- eState

    let
      mResult :: Maybe { newState :: GameState, move_src :: Maybe Location, move_dest :: Maybe Location }
      mResult = do
        piece <- state.pieces !! index
        possibleMovements <- pure $ getAllMovements state.player piece.location piece.info.movements # (filter locationInBounds)

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

                newState <- pure $ (updateTurnActions state)
                  { pieces = piecesAfterCapture
                  , capturedPieces = state.capturedPieces <> [ { info: destPiece.info, player: state.currentPlayer } ]
                  , activePieceIndex = Nothing
                  , capturedPanel { currentPageCount = newPageCount }
                  }

                Just
                  { newState: newState
                  , move_src: Just piece.location
                  , move_dest: Just clickLocation
                  }
              else
                Just
                  { newState: state { activePieceIndex = Nothing }
                  , move_src: Nothing
                  , move_dest: Nothing
                  }
            Nothing ->
              Just
                { newState: (updateTurnActions state) { pieces = piecesAfterMove, activePieceIndex = Nothing }
                , move_src: Just piece.location
                , move_dest: Just clickLocation
                }
        else
          Just
            { newState: state { activePieceIndex = Nothing }
            , move_src: Nothing
            , move_dest: Nothing
            }

    case mResult of
      Just result ->
        case result.move_src, result.move_dest of
          Just src, Just dest -> do
            send $ JSON.writeJSON $ createMovePayload result.newState src dest
            pure result.newState
          _, _ -> pure result.newState
      Nothing -> pure state

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

  checkClickBoard :: Effect GameState -> Effect GameState
  checkClickBoard eState = do
    state <- eState

    let
      clickLocation = case state.player of
        Player1 -> posToLocation y x
        Player2 -> mirrorLocation $ posToLocation y x

    log $ show clickLocation

    case state.activePieceIndex, state.activeCapturedPieceIndex of
      Just index, Nothing -> movePiece clickLocation index eState
      Nothing, Just index -> placeCapturedPiece clickLocation index eState
      _, _ -> pure $ selectPiece clickLocation state

onKeyDown :: (String -> Effect Unit) -> String -> GameState -> Effect GameState
onKeyDown send key gameState = do
  send $ "I pressed " <> key
  pure gameState

onKeyUp :: (String -> Effect Unit) -> String -> GameState -> Effect GameState
onKeyUp _ _ gameState = pure gameState

onMessage :: (String -> Effect Unit) -> Message -> GameState -> Effect GameState
onMessage _ message gameState = do
  log $ "Received message: " <> show message

  -- This method is not particularly extendable,
  -- but since this isn't a requirement I'll just leave it as is
  (pure gameState)
    <#> receiveMoveMessage
    <#> receivePlaceMessage
    <#> receivePingMessage

  where
  receiveMoveMessage :: GameState -> GameState
  receiveMoveMessage state =
    case JSON.readJSON message.payload of
      Right (payload :: MovePayload) ->
        if not (isSamePlayer message.playerId state.player) && payload.message_type == "move" then
          handleMoveMessage payload state
        else state
      _ -> state

  handleMoveMessage :: MovePayload -> GameState -> GameState
  handleMoveMessage payload state =
    let
      { move_src, move_dest } = payload.message_content
      src = move_src
      dest = move_dest

      initialPieces = state.pieces

      mNewState = do
        piece <- getPieceAtLocation initialPieces src
        index <- getPieceIndex initialPieces piece
        piecesAfterMove <- updateAt index (piece { location = dest }) initialPieces

        case getPieceAtLocation initialPieces dest of
          Just capturedPiece -> do
            capturedIndex <- getPieceIndex initialPieces capturedPiece
            piecesAfterCapture <- deleteAt capturedIndex piecesAfterMove
            pure $ (updateTurnActions state) { pieces = piecesAfterCapture }
          Nothing ->
            pure $ (updateTurnActions state) { pieces = piecesAfterMove }
    in
      case mNewState of
        Just newState -> newState
        Nothing -> state

  receivePlaceMessage :: GameState -> GameState
  receivePlaceMessage state =
    case JSON.readJSON message.payload of
      Right (payload :: PlacePayload) ->
        if not (isSamePlayer message.playerId state.player) && payload.message_type == "place" then
          handlePlaceMessage payload message.playerId state
        else state
      _ -> state

  handlePlaceMessage :: PlacePayload -> PlayerId -> GameState -> GameState
  handlePlaceMessage payload player state =
    let
      { place_piece_kind, place_dest } = payload.message_content
      dest = place_dest

      newState = case pieceKindFromString place_piece_kind of
        Just pieceKind ->
          let
            newPiece = createPiece pieceKind player dest
          in
            (updateTurnActions state) { pieces = state.pieces <> [ newPiece ] }
        Nothing -> state
    in
      newState

  -- Not a reliable method of handling connections
  -- It would be better if I had direct access to the game engine methods
  -- However, this should suffice at least
  receivePingMessage :: GameState -> GameState
  receivePingMessage state =
    if not state.receivedPing then
      case JSON.readJSON message.payload of
        Right (payload :: PingPayload) ->
          if payload.message_type == "ping" then
            state { player = message.playerId, receivedPing = true }
          else state
        _ -> state
    else state

onRender :: Map.Map String Canvas.CanvasImageSource -> Canvas.Context2D -> GameState -> Effect Unit
onRender images ctx gameState = do
  renderGame

  where
  renderGame :: Effect Unit
  renderGame = do
    clearCanvas ctx { color: "black", width, height }
    renderBoard gameState.player gameState.pieces gameState.activePieceIndex
    renderCapturedPanel gameState.capturedPanel gameState.capturedPieces gameState.activeCapturedPieceIndex
    renderGameOver gameState.gameOverState

  renderTile :: Int -> Int -> Effect Unit
  renderTile r c =
    let
      color =
        if mod (r + c) 2 == 0 then
          "lightgreen"
        else
          "lightyellow"
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

  renderGameOver :: GameOverState -> Effect Unit
  renderGameOver gameOverState =
    let
      mGameOverText = case gameOverState of
        Winner winner ->
          case winner of
            Player1 -> Just "Player 1 (Orange) Wins"
            Player2 -> Just "Player 2 (Blue) Wins"
        Draw -> Just "Draw"
        None -> Nothing

      size = 18
    in
      case mGameOverText of
        Just text -> do
          drawRect ctx
            { x: 0.0
            , y: 0.0
            , width
            , height
            , color: "#000000AA"
            }

          drawText ctx
            { x: width / 2.0
            , y: height / 2.0 + (toNumber size) / 2.0
            , text
            , color: "white"
            , font: "arial"
            , size
            }
        Nothing -> pure unit

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
