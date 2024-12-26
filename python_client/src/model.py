from __future__ import annotations
from dataclasses import dataclass, field
from typing import Protocol

from project_types import Player, PieceKind, Location, PieceData

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
    _data: PieceData
    _movement: Movement
    _is_protected: bool
    _can_capture: bool

    @property
    def data(self) -> PieceData: ...

    def can_move(self, to: Location) -> bool: ...

    # todo: where should move() go? (see BoardGameModel)
    # def move(self, to: Location): ...


@dataclass
class RegularPiece:
    _data: PieceData
    _movement: Movement
    _is_protected: bool = field(init=False, default=False)
    _can_capture: bool = field(init=False, default=True)

    @property
    def data(self) -> PieceData:
        return self._data

    def can_move(self, to: Location) -> bool:
        return to - self._data.location in self._movement.get_deltas()

    def capture(self) -> None:
        pass


@dataclass
class ProtectedPiece:
    _data: PieceData
    _movement: Movement
    _is_protected: bool = field(init=False, default=True)
    _can_capture: bool = field(init=False, default=False)

    @property
    def data(self) -> PieceData:
        return self._data

    def can_move(self, to: Location) -> bool:
        return to - self._data.location in self._movement.get_deltas()


"""
class Pawn(RegularPiece):
    def __init__(self, location: Location) -> None:
        super().__init__(PieceData(PieceKind.PAWN, location), PawnMovement())


class King(ProtectedPiece):
    def __init__(self, location: Location) -> None:
        super().__init__(PieceData(PieceKind.KING, location), KingMovement())


class Lance(RegularPiece):
    def __init__(self, location: Location) -> None:
        super().__init__(PieceData(PieceKind.LANCE, location), LanceMovement())
"""


# --- MARK: PieceFactory


class PieceFactory:
    @classmethod
    def make(cls, piece_kind: PieceKind, location: Location) -> Piece:
        match piece_kind:
            case PieceKind.PAWN:
                return RegularPiece(PieceData(PieceKind.PAWN, location), PawnMovement())
            case PieceKind.KING:
                return ProtectedPiece(
                    PieceData(PieceKind.KING, location), KingMovement()
                )
            case PieceKind.LANCE:
                return RegularPiece(
                    PieceData(PieceKind.LANCE, location), LanceMovement()
                )


# --- MARK: Board

type PlayerPiece = tuple[Player, Piece]


@dataclass
class Board:
    _rows: int = field(default=8)
    _columns: int = field(default=8)
    _pieces: dict[Location, PlayerPiece | None] = field(init=False)

    @property
    def rows(self) -> int:
        return self._rows

    @property
    def columns(self) -> int:
        return self._columns

    def add_piece(self, player: Player, piece: Piece) -> None:
        if not self.is_square_within_bounds(piece.data.location):
            raise IndexError(
                "Error: Attempted to add a piece to an out of bounds location on the board."
            )

        if not self.is_square_empty(piece.data.location):
            raise Exception(
                "Error: Attempted to add a piece to a non-empty board square."
            )

        self._pieces[piece.data.location] = (player, piece)

    def get_player_piece(self, location: Location) -> PlayerPiece | None:
        if not self.is_square_within_bounds(location):
            raise IndexError(
                "Error: Attempted to get a piece from an out of bounds location on the board."
            )

        return self._pieces[location]

    def remove_piece(self, location: Location) -> None:
        if not self.is_square_within_bounds(location):
            raise IndexError(
                "Error: Attempted to remove a piece from an out of bounds location on the board."
            )

        if self.is_square_empty(location):
            raise Exception(
                "Error: Attempted to remove a piece from an empty board square."
            )

        self._pieces.pop(location)

    def is_square_empty(self, location: Location) -> bool:
        return location not in self._pieces

    def is_square_within_bounds(self, location: Location) -> bool:
        return 1 <= location.row <= self._rows and 1 <= location.column <= self._columns


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


# --- MARK: Board Game Model


# todo: rename BoardGameModel with actual board game name
class BoardGameModel:
    _board: Board
    _piece_positions: PiecePositions

    def __init__(self, board: Board) -> None:
        self._board = board
        self._piece_positions = BoardGamePiecePositions()

        self._setup_board()

    def _setup_board(self):
        for (
            location,
            player_piece_kind,
        ) in self._piece_positions.get_positions().items():
            player: Player = player_piece_kind[0]
            piece_kind: PieceKind = player_piece_kind[1]
            piece: Piece = PieceFactory.make(piece_kind, location)
            self._board.add_piece(player, piece)

    # todo 1: where should move() go? (see Piece)
    # todo 2: where should Player info be stored?
    # todo 3: check if Player can move a certain piece
    # todo 4: check if a piece can move to another square (with minimal coupling) (how to tell a piece that there's a piece that cannot be captured?)
    def move(self) -> None:
        pass
