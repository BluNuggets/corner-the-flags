from __future__ import annotations
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, replace
from enum import StrEnum
from typing import Protocol

# --- MARK: GameStatus


class GameStatus(StrEnum):
    ONGOING = "Ongoing"
    DRAW = "Draw"
    PLAYER_1_WIN = "Player 1 wins"
    PLAYER_2_WIN = "Player 2 wins"


# --- MARK: GameState


@dataclass(frozen=True)
class GameState:
    # "constants"
    max_moves: int
    # fields
    captured_pieces: Mapping[Player, Sequence[PieceKind]]
    player_to_move: Player
    turn: int
    move: int

    def new_game(self) -> GameState:
        return replace(
            self, captured_pieces={}, player_to_move=Player.PLAYER_1, turn=1, move=1
        )

    def next_move(self) -> GameState:
        if self.move < GameState.max_moves:
            return replace(self, move=self.move + 1)
        else:
            match self.player_to_move:
                case Player.PLAYER_1:
                    return replace(self, player_to_move=Player.PLAYER_2, move=1)
                case Player.PLAYER_2:
                    return replace(
                        self, player_to_move=Player.PLAYER_1, turn=self.turn + 1, move=1
                    )


# --- MARK: Player


class Player(StrEnum):
    PLAYER_1 = "Player 1"
    PLAYER_2 = "Player 2"


# --- MARK: PieceKind


class PieceKind(StrEnum):
    PAWN = "Pawn"
    KING = "King"
    LANCE = "Lance"


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
    _piece_kind: PieceKind
    _location: Location

    @property
    def piece_kind(self) -> PieceKind: ...

    @property
    def location(self) -> Location: ...


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
