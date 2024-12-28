from __future__ import annotations
import copy
from dataclasses import dataclass, field, replace
from typing import Protocol, Self

from project_types import (
    Player,
    PieceKind,
    Location,
    PiecePositions,
    BoardGamePiecePositions,
    GameState,
    Feedback,
    FeedbackInfo
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


class PieceFactory:
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
    _rows: int = field(default=8)
    _columns: int = field(default=8)
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
            raise IndexError(
                "Error: Attempted to add a piece to an out of bounds location on the board."
            )

        if not self.is_square_empty(piece.location):
            raise Exception(
                "Error: Attempted to add a piece to a non-empty board square."
            )

        self._pieces[piece.location] = piece

    def get_piece(self, location: Location) -> Piece | None:
        if not self.is_square_within_bounds(location):
            raise IndexError(
                "Error: Attempted to get a piece from an out of bounds location on the board."
            )
        
        return self._pieces[location] if self._pieces.get(location) else None

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
    _state: GameState

    #set default settings of Board
    @classmethod
    def default_board(cls) -> Self:
        state = GameState(
            current_player = Player.PLAYER_1,
            board_dimension = (8,8),
        )

        return cls(Board(8,8), state)
    
    def __init__(self, board: Board, state: GameState) -> None:
        self._board = board
        self._piece_positions = BoardGamePiecePositions()
        self._state = state

        self._setup_board()

    def _setup_board(self):
        for (
            location,
            player_piece_kind,
        ) in self._piece_positions.get_positions().items():
            player: Player = player_piece_kind[0]
            piece_kind: PieceKind = player_piece_kind[1]
            piece: Piece = PieceFactory.make(player, piece_kind, location)
            self._board.add_piece(piece)

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
    
    def move_from_view(self, src: Location, dest: Location, player: Player) -> Feedback:
        if self.can_move(src, dest):
            # todo: update state as well
            self.move(src, dest)
            return Feedback(
                move = (src, dest),
                info = FeedbackInfo.VALID
            )
        elif player != self._state.current_player:
            return Feedback(
                move = (src, dest),
                info = FeedbackInfo.NOT_CURRENT_PLAYER
            )
        else:
            return Feedback(
                move = (src, dest),
                info = FeedbackInfo.INVALID
            )
        
    # todo: implement piece capture logic

    @property
    def state(self) -> GameState:
        return replace(self._state)
