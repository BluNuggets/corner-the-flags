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
    MoveFeedback,
    MoveFeedbackInfo,
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
        return self._player

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
            raise ValueError(f'Error: RegularPiece cannot move to square {dest}')

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
        return self._player

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
            raise ValueError(f'Error: RegularPiece cannot move to square {dest}')

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
                'Error: Attempted to add a piece to an out of bounds location on the board.'
            )

        if not self.is_square_empty(piece.location):
            raise KeyError(
                'Error: Attempted to add a piece to a non-empty board square.'
            )

        self._pieces[piece.location] = piece

    def get_piece(self, location: Location) -> Piece | None:
        if not self.is_square_within_bounds(location):
            raise KeyError(
                'Error: Attempted to get a piece from an out of bounds location on the board.'
            )

        return self._pieces.get(location, None)

    def remove_piece(self, location: Location) -> None:
        if not self.is_square_within_bounds(location):
            raise KeyError(
                'Error: Attempted to remove a piece from an out of bounds location on the board.'
            )

        if self.is_square_empty(location):
            raise KeyError(
                'Error: Attempted to remove a piece from an empty board square.'
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
        captured_pieces: dict[Player, list[PieceKind]] = {
            Player.PLAYER_1: [],
            Player.PLAYER_2: [],
        }
        state: BoardGameFrozenGameState = BoardGameFrozenGameState(
            _max_moves=MAX_MOVES, _player_to_move=Player.PLAYER_1, _turn=1, _move=1
        )

        return cls(
            player_id,
            board,
            captured_pieces,
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
                raise ValueError('Error: BoardGameModel received invalid player ID.')
        self._board = board
        self._captured_pieces = captured_pieces
        self._state = state
        self._board_setter = BoardSetter(piece_positions, piece_factory)

        self._board_setter.setup_board(self._board)

    @property
    def player(self) -> Player:
        return self._player

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
        self._captured_pieces = {
            Player.PLAYER_1: [],
            Player.PLAYER_2: [],
        }

        self._state = replace(
            self._state, _player_to_move=Player.PLAYER_1, _turn=1, _move=1
        )

    def next_move(self) -> None:
        # update turn/move number
        if self.move < self.max_moves:
            self._state = replace(
                self._state,
                _move=self._state.move + 1,
            )
        else:
            self._state = replace(
                self._state,
                _turn=self._state.turn + 1,
                _move=1,
            )

            # change player to move
            match self.player_to_move:
                case Player.PLAYER_1:
                    self._state = replace(self._state, _player_to_move=Player.PLAYER_2)
                case Player.PLAYER_2:
                    self._state = replace(self._state, _player_to_move=Player.PLAYER_1)

    def get_move_feedback_info(
        self, src: Location, dest: Location, player: Player
    ) -> MoveFeedbackInfo:
        # not currently player's turn to move
        if self._state.player_to_move != player:
            return MoveFeedbackInfo.NOT_CURRENT_PLAYER

        # src Location does not exist in board
        if not self._board.is_square_within_bounds(src):
            return MoveFeedbackInfo.SQUARE_OUT_OF_BOUNDS

        # dest Location does not exist in board
        if not self._board.is_square_within_bounds(dest):
            return MoveFeedbackInfo.SQUARE_OUT_OF_BOUNDS

        # ---

        src_piece: Piece | None = self._board.get_piece(src)

        # no piece exists in src Location
        if src_piece is None:
            return MoveFeedbackInfo.NO_PIECE_MOVED

        # piece in src Location does not belong to the player
        # todo: uncomment if ready to test with 2 clients
        if src_piece.player != player:
            return MoveFeedbackInfo.PIECE_DOES_NOT_BELONG_TO_PLAYER

        # piece in src Location cannot reach dest Location
        if not src_piece.can_move(dest):
            return MoveFeedbackInfo.PIECE_CANNOT_REACH_SQUARE

        # ---

        # a piece can always move to an empty dest Location
        dest_piece: Piece | None = self._board.get_piece(dest)
        if dest_piece is None:
            return MoveFeedbackInfo.VALID

        # a player cannot capture their own piece
        if src_piece.player == dest_piece.player:
            return MoveFeedbackInfo.CAPTURES_OWN_PIECE
        # a player can capture an opponent piece if it is not a protected piece
        else:
            if dest_piece.is_protected:
                return MoveFeedbackInfo.CAPTURES_PROTECTED_PIECE
            else:
                return MoveFeedbackInfo.VALID

    def is_move_valid(self, src: Location, dest: Location, player: Player) -> bool:
        return self.get_move_feedback_info(src, dest, player) == MoveFeedbackInfo.VALID

    def _make_valid_move_feedback(
        self, src: Location, dest: Location, player: Player
    ) -> MoveFeedback:
        return MoveFeedback(
            move_src=src,
            move_dest=dest,
            info=self.get_move_feedback_info(src, dest, player),
        )

    def _make_invalid_move_feedback(
        self, src: Location, dest: Location, player: Player
    ) -> MoveFeedback:
        return MoveFeedback(
            move_src=src,
            move_dest=None,
            info=self.get_move_feedback_info(src, dest, player),
        )

    def move_piece(self, src: Location, dest: Location, player: Player) -> MoveFeedback:
        if not self.is_move_valid(src, dest, player):
            return self._make_invalid_move_feedback(src, dest, player)
        else:
            ret: MoveFeedback = self._make_valid_move_feedback(src, dest, player)

            # ---

            src_piece: Piece | None = self._board.get_piece(src)
            if src_piece is None:
                raise Exception(f'Error: Move was called from empty square {src}.')

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

            # update piece with new position
            src_piece.move(dest)

            # add piece to new position
            self._board.add_piece(src_piece)

            # clear old position from board
            self._board.remove_piece(src)

            # ---

            self.next_move()

            return ret
