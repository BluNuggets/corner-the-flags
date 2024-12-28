from __future__ import annotations
import copy
from dataclasses import dataclass, field
from typing import Protocol

from project_types import (
    GameState,
    Player,
    PieceKind,
    Location,
    PiecePositions,
    BoardGamePiecePositions,
)

# --- MARK: Movement


class Movement(Protocol):
    def get_deltas(self) -> set[Location]: ...


class PlayerOnePawnMovement:
    def get_deltas(self) -> set[Location]:
        return {Location(1, 0)}


class PlayerTwoPawnMovement:
    def get_deltas(self) -> set[Location]:
        return {Location(-1, 0)}


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
    _player: Player
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool
    _can_capture: bool

    @property
    def player(self) -> Player: ...

    @property
    def piece_kind(self) -> PieceKind: ...

    @property
    def location(self) -> Location: ...

    @property
    def is_protected(self) -> bool: ...

    @property
    def can_capture(self) -> bool: ...

    def can_move(self, dest: Location) -> bool: ...

    def move(self, dest: Location): ...


@dataclass
class RegularPiece:
    _player: Player
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=False)
    _can_capture: bool = field(init=False, default=True)

    @property
    def player(self) -> Player:
        return self.player

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

    def can_move(self, dest: Location) -> bool:
        return dest - self._location in self._movement.get_deltas()

    def move(self, dest: Location) -> None:
        if not self.can_move(dest):
            raise ValueError(f"Error: RegularPiece cannot move to square {dest}")

        self._location = copy.copy(dest)


@dataclass
class ProtectedPiece:
    _player: Player
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=True)
    _can_capture: bool = field(init=False, default=False)

    @property
    def player(self) -> Player:
        return self.player

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

    def can_move(self, dest: Location) -> bool:
        return dest - self._location in self._movement.get_deltas()

    def move(self, dest: Location) -> None:
        if not self.can_move(dest):
            raise ValueError(f"Error: RegularPiece cannot move to square {dest}")

        self._location = copy.copy(dest)


# --- MARK: PieceFactory


class PieceFactory(Protocol):
    @classmethod
    def make(
        cls, player: Player, piece_kind: PieceKind, location: Location
    ) -> Piece: ...


class BoardGamePieceFactory:
    @classmethod
    def make(cls, player: Player, piece_kind: PieceKind, location: Location) -> Piece:
        match piece_kind:
            case PieceKind.PAWN:
                match player:
                    case Player.PLAYER_1:
                        return RegularPiece(
                            player, PieceKind.PAWN, location, PlayerOnePawnMovement()
                        )
                    case Player.PLAYER_2:
                        return RegularPiece(
                            player, PieceKind.PAWN, location, PlayerTwoPawnMovement()
                        )
            case PieceKind.KING:
                return ProtectedPiece(player, PieceKind.KING, location, KingMovement())
            case PieceKind.LANCE:
                return RegularPiece(player, PieceKind.LANCE, location, LanceMovement())


# --- MARK: Board


@dataclass
class Board:
    _rows: int
    _columns: int
    _pieces: dict[Location, Piece] = field(default_factory=dict)

    @property
    def rows(self) -> int:
        return self._rows

    @property
    def columns(self) -> int:
        return self._columns

    @property
    def pieces(self) -> dict[Location, Piece]:
        return self._pieces

    def add_piece(self, piece: Piece) -> None:
        if not self.is_square_within_bounds(piece.location):
            raise KeyError(
                "Error: Attempted to add a piece to an out of bounds location on the board."
            )

        if not self.is_square_empty(piece.location):
            raise KeyError(
                "Error: Attempted to add a piece to a non-empty board square."
            )

        self._pieces[piece.location] = piece

    def get_piece(self, location: Location) -> Piece | None:
        if not self.is_square_within_bounds(location):
            raise KeyError(
                "Error: Attempted to get a piece from an out of bounds location on the board."
            )

        return self._pieces[location]

    def remove_piece(self, location: Location) -> None:
        if not self.is_square_within_bounds(location):
            raise KeyError(
                "Error: Attempted to remove a piece from an out of bounds location on the board."
            )

        if self.is_square_empty(location):
            raise KeyError(
                "Error: Attempted to remove a piece from an empty board square."
            )

        self._pieces.pop(location)

    def is_square_empty(self, location: Location) -> bool:
        return location not in self._pieces

    def is_square_within_bounds(self, location: Location) -> bool:
        return 1 <= location.row <= self._rows and 1 <= location.column <= self._columns


# --- MARK: BoardSetter


class BoardSetter:
    _piece_positions: PiecePositions
    _piece_factory: PieceFactory

    def __init__(
        self, piece_positions: PiecePositions, piece_factory: PieceFactory
    ) -> None:
        self._piece_positions = piece_positions
        self._piece_factory = piece_factory

    def setup_board(self, board: Board):
        for (
            location,
            player_piece_kind,
        ) in self._piece_positions.get_positions().items():
            player: Player = player_piece_kind[0]
            piece_kind: PieceKind = player_piece_kind[1]
            piece: Piece = self._piece_factory.make(player, piece_kind, location)
            board.add_piece(piece)


# --- MARK: Board Game Model


class Model(Protocol):
    _state: GameState
    _board: Board
    _piece_positions: PiecePositions

    @classmethod
    def setup_game(cls) -> Model: ...

    def can_move(self, src: Location, dest: Location) -> bool: ...
    def move(self, src: Location, dest: Location): ...


# todo: rename BoardGameModel with actual board game name
class BoardGameModel:
    # _player: Player
    _state: GameState
    _board: Board
    _board_setter: BoardSetter

    @classmethod
    def setup_game(cls) -> BoardGameModel:
        state = GameState(
            captured_pieces={}, player_to_move=Player.PLAYER_1, turn=1, move=1
        )

        return cls(
            state, Board(8, 8), BoardGamePiecePositions(), BoardGamePieceFactory()
        )

    def __init__(
        self,
        state: GameState,
        board: Board,
        piece_positions: PiecePositions,
        piece_factory: PieceFactory,
    ) -> None:
        self._state = state
        self._board = board
        self._board_setter = BoardSetter(piece_positions, piece_factory)

        self._board_setter.setup_board(self._board)

    # todo 1: check if Player can move a certain piece
    # todo 2: possibly add player to model fields
    def can_move(self, src: Location, dest: Location) -> bool:
        # src Location does not exist in board
        if not self._board.is_square_within_bounds(src):
            return False

        # dest Location does not exist in board
        if not self._board.is_square_within_bounds(dest):
            return False

        # ---

        src_piece: Piece | None = self._board.get_piece(src)

        # no piece exists in src Location
        if src_piece is None:
            return False

        # piece in src Location cannot reach dest Location
        if not src_piece.can_move(dest):
            return False

        # ---

        # a piece can always move to an empty dest Location
        dest_piece: Piece | None = self._board.get_piece(dest)
        if dest_piece is None:
            return True

        # a player cannot capture their own piece
        if src_piece.player == dest_piece.player:
            return False
        # a player can capture an opponent piece if it is not a protected piece
        else:
            return not dest_piece.is_protected

    def move(self, src: Location, dest: Location) -> None:
        if not self.can_move(src, dest):
            raise Exception("Error: Invalid move was sent.")

        src_piece: Piece | None = self._board.get_piece(src)
        if src_piece is None:
            raise Exception(f"Error: Move was called from empty square {src}.")

        src_piece.move(dest)

        # ---

        dest_piece: Piece | None = self._board.get_piece(dest)
        if dest_piece is None:
            return

        # todo: implement piece capture logic
