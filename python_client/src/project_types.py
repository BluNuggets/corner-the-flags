from __future__ import annotations
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from enum import StrEnum
from typing import Protocol, TypedDict

# --- MARK: GameStatus


class GameStatus(StrEnum):
    ONGOING = 'Ongoing'
    DRAW = 'Draw'
    PLAYER_1_WIN = 'Player 1 wins'
    PLAYER_2_WIN = 'Player 2 wins'


# --- MARK: GameState


# basically a ReadOnly(Model+State)
class GameState(Protocol):
    @property
    def player(self) -> Player: ...

    @property
    def max_moves(self) -> int: ...

    @property
    def board(self) -> BoardData: ...

    @property
    def captured_pieces(self) -> Mapping[Player, Sequence[PieceKind]]: ...

    @property
    def player_to_move(self) -> Player: ...

    @property
    def turn(self) -> int: ...

    @property
    def move(self) -> int: ...

    @property
    def game_status(self) -> GameStatus: ...


# --- MARK: Player


class Player(StrEnum):
    PLAYER_1 = 'Player 1'
    PLAYER_2 = 'Player 2'


# --- MARK: PieceKind


class PieceKind(StrEnum):
    PAWN = 'Pawn'
    KING = 'King'
    LANCE = 'Lance'


# --- MARK: Location


@dataclass(frozen=True)
class Location:
    _row: int
    _column: int

    def __copy__(self) -> Location:
        return Location(self._row, self._column)

    def __hash__(self) -> int:
        """Hash a Location() instance by hashing a tuple[int,int] that contains the values of .row and .column"""
        return hash((self._row, self._column))

    def __sub__(self, other: Location) -> Location:
        """Get the difference of two Location() instances"""
        return Location(self._row - other.row, self._column - other.column)

    @property
    def row(self) -> int:
        return self._row

    @property
    def column(self) -> int:
        return self._column


# --- MARK: PieceData


class PieceData(Protocol):
    @property
    def piece_kind(self) -> PieceKind: ...

    @property
    def location(self) -> Location: ...


# --- MARK: BoardData


class BoardData(Protocol):
    @property
    def rows(self) -> int: ...

    @property
    def columns(self) -> int: ...

    @property
    def pieces(self) -> Mapping[Location, PieceData]: ...


# todo: does this stay here in project_types?
# --- MARK: PiecePositions


class PiecePositions(Protocol):
    def get_positions(self) -> dict[Location, tuple[Player, PieceKind]]: ...


class BoardGamePiecePositions:
    def get_positions(self) -> dict[Location, tuple[Player, PieceKind]]:
        return {
            # Player 1
            Location(2, 1): (Player.PLAYER_1, PieceKind.PAWN),
            Location(2, 2): (Player.PLAYER_1, PieceKind.PAWN),
            Location(2, 3): (Player.PLAYER_1, PieceKind.PAWN),
            Location(2, 4): (Player.PLAYER_1, PieceKind.PAWN),
            Location(2, 5): (Player.PLAYER_1, PieceKind.PAWN),
            Location(2, 6): (Player.PLAYER_1, PieceKind.PAWN),
            Location(2, 7): (Player.PLAYER_1, PieceKind.PAWN),
            Location(2, 8): (Player.PLAYER_1, PieceKind.PAWN),
            Location(1, 1): (Player.PLAYER_1, PieceKind.KING),
            # Player 2
            Location(7, 1): (Player.PLAYER_2, PieceKind.PAWN),
            Location(7, 2): (Player.PLAYER_2, PieceKind.PAWN),
            Location(7, 3): (Player.PLAYER_2, PieceKind.PAWN),
            Location(7, 4): (Player.PLAYER_2, PieceKind.PAWN),
            Location(7, 5): (Player.PLAYER_2, PieceKind.PAWN),
            Location(7, 6): (Player.PLAYER_2, PieceKind.PAWN),
            Location(7, 7): (Player.PLAYER_2, PieceKind.PAWN),
            Location(7, 8): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 1): (Player.PLAYER_2, PieceKind.KING),
        }


# --- MARK: MoveFeedbackInfo


class MoveFeedbackInfo(StrEnum):
    NOT_CURRENT_PLAYER = 'Not current player'
    NO_PIECE_MOVED = 'No piece moved'
    SQUARE_OUT_OF_BOUNDS = 'Square out of bounds'
    PIECE_DOES_NOT_BELONG_TO_PLAYER = 'Piece does not belong to player'
    PIECE_CANNOT_REACH_SQUARE = 'Piece cannot reach square'
    CAPTURES_OWN_PIECE = 'Captures own piece'
    CAPTURES_PROTECTED_PIECE = 'Captures protected piece'
    VALID = 'Valid'


@dataclass(frozen=True)
class MoveFeedback:
    move_src: Location
    move_dest: Location | None
    info: MoveFeedbackInfo


# --- MARK: PlaceFeedbackInfo


class PlaceFeedbackInfo(StrEnum):
    NOT_CURRENT_PLAYER = 'Not current player'
    NO_PIECE_MOVED = 'No piece moved'
    SQUARE_OUT_OF_BOUNDS = 'Square out of bounds'
    PIECE_DOES_NOT_BELONG_TO_PLAYER = 'Piece does not belong to player'
    PIECE_CANNOT_REACH_SQUARE = 'Piece cannot reach square'
    CAPTURES_OWN_PIECE = 'Captures own piece'
    CAPTURES_PROTECTED_PIECE = 'Captures protected piece'
    VALID = 'Valid'


@dataclass(frozen=True)
class PlaceFeedback:
    place_piece_kind: PieceKind
    place_dest: Location | None
    info: PlaceFeedbackInfo


# --- MARK: GameMessageDict


class GameMessageDict(TypedDict, total=True):
    frame: int
    message_type: GameMessageType
    message_content: GameMessageContentDict


# --- MARK: GameMessageType


class GameMessageType(StrEnum):
    MOVE = 'move'
    PLACE = 'place'
    INVALID = 'invalid'

    @classmethod
    def _missing_(cls, value: object) -> GameMessageType:
        return GameMessageType.INVALID


# --- MARK: GameMessageContentDict


class LocationDict(TypedDict):
    row: int
    column: int


class GameMessageContentDict(TypedDict, total=False):
    player: Player
    move_src: LocationDict
    move_dest: LocationDict
    place_piece_kind: PieceKind
    place_dest: LocationDict


class MakeMoveGameMessageContentDict(GameMessageContentDict, total=False):
    player: Player
    move_src: LocationDict
    move_dest: LocationDict


class PlacePieceGameMessageContentDict(GameMessageContentDict, total=False):
    player: Player
    place_piece_kind: PieceKind
    place_dest: LocationDict
