from __future__ import annotations
from cs150241project_networking.main import PlayerId
import copy
from dataclasses import dataclass, field, replace
from typing import Protocol

from project_types import (
    GameStatus,
    Player,
    PieceKind,
    Location,
    PiecePositions,
    BoardGamePiecePositions,
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
    _pieces: dict[Location, Piece] = field(init=False, default_factory=dict)

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


# --- MARK: BoardGameFrozenGameState


# todo: finalize distinction between Model and State
# possibly combine into a single class, depending on following "simplicity" vs following SRP
# board is generally meant to update dict-wise (mutable except as property output, add/remove dict keys)
# player_to_move, turn, move are generally meant to update state-wise (immutable, "increment" every turn)
@dataclass(frozen=True)
class BoardGameFrozenGameState:
    _max_moves: int
    _player_to_move: Player
    _turn: int
    _move: int

    @property
    def max_moves(self) -> int:
        return self._max_moves

    @property
    def player_to_move(self) -> Player:
        return self._player_to_move

    @property
    def turn(self) -> int:
        return self._turn

    @property
    def move(self) -> int:
        return self._move


# --- MARK: Board Game Model


# todo: rename BoardGameModel with actual board game name
class BoardGameModel:
    # non-state "constants"
    _player: Player
    # state
    _board: Board
    _captured_pieces: dict[Player, list[PieceKind]]
    _state: BoardGameFrozenGameState
    # non-state extras
    _board_setter: BoardSetter

    @classmethod
    def setup_game(cls, player_id: PlayerId) -> BoardGameModel:
        MAX_MOVES: int = 3
        board: Board = Board(8, 8)
        state: BoardGameFrozenGameState = BoardGameFrozenGameState(
            _max_moves=MAX_MOVES, _player_to_move=Player.PLAYER_1, _turn=1, _move=1
        )

        return cls(
            player_id,
            board,
            {},
            state,
            BoardGamePiecePositions(),
            BoardGamePieceFactory(),
        )

    def __init__(
        self,
        player_id: PlayerId,
        board: Board,
        captured_pieces: dict[Player, list[PieceKind]],
        state: BoardGameFrozenGameState,
        piece_positions: PiecePositions,
        piece_factory: PieceFactory,
    ) -> None:
        match player_id:
            case 1:
                self._player = Player.PLAYER_1
            case 2:
                self._player = Player.PLAYER_2
            case _:
                raise ValueError("Error: BoardGameModel received invalid player ID.")
        self._board = board
        self._captured_pieces = captured_pieces
        self._state = state
        self._board_setter = BoardSetter(piece_positions, piece_factory)

        self._board_setter.setup_board(self._board)

    @property
    def max_moves(self) -> int:
        return self._state.max_moves

    @property
    def board(self) -> Board:
        return self._board

    @property
    def captured_pieces(self) -> dict[Player, list[PieceKind]]:
        return self._captured_pieces

    @property
    def player_to_move(self) -> Player:
        return self._state.player_to_move

    @property
    def turn(self) -> int:
        return self._state.turn

    @property
    def move(self) -> int:
        return self._state.move

    @property
    def game_status(self) -> GameStatus:
        if len(self.protected_pieces(Player.PLAYER_1)) > 0:
            if len(self.protected_pieces(Player.PLAYER_2)):
                return GameStatus.ONGOING
            else:
                return GameStatus.PLAYER_1_WIN
        else:
            if len(self.protected_pieces(Player.PLAYER_2)):
                return GameStatus.PLAYER_2_WIN
            else:
                return GameStatus.DRAW

    def protected_pieces(self, player: Player) -> list[Piece]:
        return [
            piece for piece in self._board.pieces.values() if piece.player == player
        ]

    def new_game(self) -> None:
        self._board_setter.setup_board(self._board)
        self._captured_pieces = {}
        replace(self._state, player_to_move=Player.PLAYER_1, turn=1, move=1)

    def next_move(self) -> None:
        if self.move < self.max_moves:
            replace(self._state, move=self.move + 1)
        else:
            match self.player_to_move:
                case Player.PLAYER_1:
                    replace(self._state, player_to_move=Player.PLAYER_2, move=1)
                case Player.PLAYER_2:
                    replace(
                        self._state,
                        player_to_move=Player.PLAYER_1,
                        turn=self.turn + 1,
                        move=1,
                    )

    def can_move_piece(self, src: Location, dest: Location) -> bool:
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

        # piece in src Location does not belong to the player
        # todo: uncomment if ready to test with 2 clients
        # if src_piece.player != self._player:
        #     return False

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



    def move_piece(self, src: Location, dest: Location) -> None:
        if not self.can_move_piece(src, dest):
            raise Exception("Error: Invalid move was sent.")

        src_piece: Piece | None = self._board.get_piece(src)
        if src_piece is None:
            raise Exception(f"Error: Move was called from empty square {src}.")

        # ---

        dest_piece: Piece | None = self._board.get_piece(dest)
        if dest_piece is not None:
            # todo: verify piece capture logic
            match dest_piece.player:
                case Player.PLAYER_1:
                    receiving_player: Player = Player.PLAYER_2
                case Player.PLAYER_2:
                    receiving_player: Player = Player.PLAYER_1

            self._captured_pieces[receiving_player].append(dest_piece.piece_kind)

            self._board.remove_piece(dest)

        # ---

        src_piece.move(dest)

    def move_from_view(self, src: Location, dest: Location, player: Player) -> Feedback:
        if self.can_move_piece(src, dest):
            # todo: update state as well
            self.move_piece(src, dest)
            return Feedback(move=(src, dest), info=FeedbackInfo.VALID)
        elif player != self._state.player_to_move:
            return Feedback(move=(src, dest), info=FeedbackInfo.NOT_CURRENT_PLAYER)
        else:
            return Feedback(move=(src, dest), info=FeedbackInfo.INVALID)
