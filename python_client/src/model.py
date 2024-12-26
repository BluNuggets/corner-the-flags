from __future__ import annotations
from dataclasses import dataclass, field
from typing import Protocol

from project_types import Player, PieceKind, Location

# --- MARK: Movement


class Movement(Protocol):
    def get_deltas(self) -> set[Location]: ...


class PawnMovement:
    def get_deltas(self) -> set[Location]:
        return {Location(1, 0)}


class KingMovement:
    def get_deltas(self) -> set[Location]:
        return {
            Location(dr, dc)
            for dr in [-1, 0, 1]
            for dc in [-1, 0, 1]
            if dr != 0 or dc != 0
        }


class LanceMovement:
    def get_deltas(self) -> set[Location]:
        return {Location(dr, dc) for dr in [-1, 1] for dc in [-1, 1]}


# --- MARK: Piece


class Piece(Protocol):
    # _board: Board
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool
    _can_capture: bool

    @property
    def location(self) -> Location:
        return self._location

    def can_move(self, to: Location) -> bool: ...

    # todo: where should move() go? (see BoardGameModel)
    # def move(self, to: Location): ...


@dataclass
class RegularPiece:
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=False)
    _can_capture: bool = field(init=False, default=True)

    @property
    def location(self) -> Location:
        return self._location

    def can_move(self, to: Location) -> bool:
        return to - self._location in self._movement.get_deltas()

    def capture(self) -> None:
        pass


@dataclass
class ProtectedPiece:
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=True)
    _can_capture: bool = field(init=False, default=False)

    @property
    def location(self) -> Location:
        return self._location

    def can_move(self, to: Location) -> bool:
        return to - self._location in self._movement.get_deltas()


class Pawn(RegularPiece):
    def __init__(self, location: Location) -> None:
        super().__init__(PieceKind.PAWN, location, PawnMovement())


class King(ProtectedPiece):
    def __init__(self, location: Location) -> None:
        super().__init__(PieceKind.KING, location, KingMovement())


class Lance(RegularPiece):
    def __init__(self, location: Location) -> None:
        super().__init__(PieceKind.LANCE, location, LanceMovement())


"""
class PieceFactory:
    @classmethod
    def make(cls, piece_kind: PieceKind, location: Location) -> Piece:
        match piece_kind:
            case PieceKind.PAWN:
                return RegularPiece(PieceKind.PAWN, location, PawnMovement())
            case PieceKind.KING:
                return ProtectedPiece(PieceKind.KING, location, KingMovement())
            case PieceKind.LANCE:
                return RegularPiece(PieceKind.LANCE, location, LanceMovement())
"""

# --- MARK: Board


@dataclass
class Board:
    _rows: int = field(default=8)
    _columns: int = field(default=8)
    _matrix: list[list[Piece | None]] = field(init=False)

    @property
    def rows(self) -> int:
        return self._rows

    @property
    def columns(self) -> int:
        return self._columns

    def add_piece(self, piece: Piece) -> None:
        if not self.is_square_empty(piece.location.row, piece.location.column):
            raise Exception(
                "Error: Attempted to add a piece to a non-empty board square."
            )

        if not self.is_square_within_bounds(piece.location.row, piece.location.column):
            raise IndexError(
                "Error: Attempted to add a piece to an out of bounds location on the board."
            )

        self._matrix[self._rows - piece.location.row][piece.location.column - 1] = piece

    def get_piece(self, row: int, column: int) -> Piece | None:
        if not self.is_square_within_bounds(row, column):
            raise IndexError(
                "Error: Attempted to get a piece from an out of bounds location on the board."
            )

        return self._matrix[self._rows - row][column - 1]

    def remove_piece(self, row: int, column: int) -> None:
        # todo 1: remove Location data from piece?
        # todo 2: should a piece Location be a Maybe (union with None)?
        self._matrix[self._rows - row][column - 1] = None

    def is_square_empty(self, row: int, column: int) -> bool:
        return self.get_piece(row, column) is None

    def is_square_within_bounds(self, row: int, column: int) -> bool:
        return 1 <= row <= self._rows and 1 <= column <= self._columns


# --- MARK: Board Game Model


# todo: rename BoardGameModel with actual board game name
class BoardGameModel:
    _board: Board
    _pieces: list[tuple[Player, Piece]]

    def __init__(self, board: Board) -> None:
        self._board = board
        self._pieces = [
            # Player 1
            (Player.PLAYER_1, Pawn(Location(2, 1))),
            (Player.PLAYER_1, Pawn(Location(2, 2))),
            (Player.PLAYER_1, Pawn(Location(2, 3))),
            (Player.PLAYER_1, Pawn(Location(2, 4))),
            (Player.PLAYER_1, Pawn(Location(2, 5))),
            (Player.PLAYER_1, Pawn(Location(2, 6))),
            (Player.PLAYER_1, Pawn(Location(2, 7))),
            (Player.PLAYER_1, Pawn(Location(2, 8))),
            (Player.PLAYER_1, King(Location(1, 1))),
            # Player 2
            (Player.PLAYER_2, Pawn(Location(7, 1))),
            (Player.PLAYER_2, Pawn(Location(7, 2))),
            (Player.PLAYER_2, Pawn(Location(7, 3))),
            (Player.PLAYER_2, Pawn(Location(7, 4))),
            (Player.PLAYER_2, Pawn(Location(7, 5))),
            (Player.PLAYER_2, Pawn(Location(7, 6))),
            (Player.PLAYER_2, Pawn(Location(7, 7))),
            (Player.PLAYER_2, Pawn(Location(7, 8))),
            (Player.PLAYER_2, King(Location(8, 1))),
        ]

    # todo 1: where should move() go? (see Piece)
    # todo 2: where should Player info be stored?
    # todo 3: check if Player can move a certain piece
    # todo 4: check if a piece can move to another square (with minimal coupling) (how to tell a piece that there's a piece that cannot be captured?)
    def move(self) -> None:
        pass
