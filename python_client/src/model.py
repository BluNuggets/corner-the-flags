from __future__ import annotations
from dataclasses import dataclass, field
from typing import Protocol

from project_types import (
    Player,
    PieceKind,
    Location,
    PiecePositions,
    BoardGamePiecePositions,
)

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
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool
    _can_capture: bool

    @property
    def piece_kind(self) -> PieceKind: ...

    @property
    def location(self) -> Location: ...

    @property
    def is_protected(self) -> bool: ...

    @property
    def can_capture(self) -> bool: ...

    def can_move(self, to: Location) -> bool: ...

    # todo: where should move() go? (see BoardGameModel)
    # def move(self, dest: Location): ...


@dataclass
class RegularPiece:
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=False)
    _can_capture: bool = field(init=False, default=True)

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def location(self) -> Location:
        return self._location

    @property
    def is_protected(self) -> bool:
        return self._is_protected

    @property
    def can_capture(self) -> bool:
        return self._can_capture

    def can_move(self, to: Location) -> bool:
        return to - self._location in self._movement.get_deltas()

    """
    def move(self, to: Location) -> None:
        if not self.can_move(to):
            raise ValueError(f'Error: RegularPiece cannot move to square {to}')
    """


@dataclass
class ProtectedPiece:
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=True)
    _can_capture: bool = field(init=False, default=False)

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def location(self) -> Location:
        return self._location

    @property
    def is_protected(self) -> bool:
        return self._is_protected

    @property
    def can_capture(self) -> bool:
        return self._can_capture

    def can_move(self, to: Location) -> bool:
        return to - self._location in self._movement.get_deltas()

    """
    def move(self, to: Location) -> None:
        if not self.can_move(to):
            raise ValueError(f'Error: RegularPiece cannot move to square {to}')
    """


"""
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


# --- MARK: PieceFactory


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


# --- MARK: Board

type PlayerPiece = tuple[Player, Piece]


@dataclass
class Board:
    _rows: int = field(default=8)
    _columns: int = field(default=8)
    _pieces: dict[Location, PlayerPiece] = field(init=False)

    @property
    def rows(self) -> int:
        return self._rows

    @property
    def columns(self) -> int:
        return self._columns

    @property
    def pieces(self) -> dict[Location, PlayerPiece]:
        return self._pieces

    def add_piece(self, player: Player, piece: Piece) -> None:
        if not self.is_square_within_bounds(piece.location):
            raise IndexError(
                "Error: Attempted to add a piece to an out of bounds location on the board."
            )

        if not self.is_square_empty(piece.location):
            raise Exception(
                "Error: Attempted to add a piece to a non-empty board square."
            )

        self._pieces[piece.location] = (player, piece)

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
    def can_move(self, src: Location, dest: Location) -> bool:
        # src Location does not exist in board
        if not self._board.is_square_within_bounds(src):
            return False

        # dest Location does not exist in board
        if not self._board.is_square_within_bounds(dest):
            return False

        # ---

        maybe_src_player_piece: PlayerPiece | None = self._board.get_player_piece(src)

        # no piece exists in src Location
        if maybe_src_player_piece is None:
            return False

        src_player: Player = maybe_src_player_piece[0]
        src_piece: Piece = maybe_src_player_piece[1]

        # piece in src Location cannot reach dest Location
        if not src_piece.can_move(dest):
            return False

        # ---

        # a piece can always move to an empty dest Location
        maybe_dest_player_piece: PlayerPiece | None = self._board.get_player_piece(dest)
        if maybe_dest_player_piece is None:
            return True

        dest_player: Player = maybe_dest_player_piece[0]
        dest_piece: Piece = maybe_dest_player_piece[1]

        # a player cannot capture their own piece
        if src_player == dest_player:
            return False
        # a player can capture an opponent piece if it is not a protected piece
        else:
            return not dest_piece.is_protected

    def move(self, src: Location, dest: Location) -> None:
        if not self.can_move(src, dest):
            raise Exception("Error: Invalid move was sent.")

        src_player_piece: PlayerPiece | None = self._board.get_player_piece(src)
        if src_player_piece is None:
            raise Exception(f"Error: Move was called from empty square {src}.")

        # todo: implement piece move logic
        # src_piece: Piece = src_player_piece[1]
        # src_piece.move(dest)

        # ---

        dest_player_piece: PlayerPiece | None = self._board.get_player_piece(dest)
        if dest_player_piece is None:
            return

        # todo: implement piece capture logic
