from __future__ import annotations
from dataclasses import dataclass, replace, field
from enum import StrEnum
from typing import Protocol, Self, TypedDict


class Player(StrEnum):
    PLAYER_1 = "Player 1"
    PLAYER_2 = "Player 2"


class PieceKind(StrEnum):
    PAWN = "Pawn"
    KING = "King"
    LANCE = "Lance"


@dataclass
class Location:
    _row: int
    _column: int

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


@dataclass
class PieceData:
    _piece_kind: PieceKind
    _location: Location

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def location(self) -> Location:
        return self._location

    """
    def move(self, to: Location) -> None:
        self._location = to
    """


type PlayerPieceData = tuple[Player, PieceData]

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


# --- MARK: Game State
# todo: what constitutes as an "invalid game state"?
@dataclass(frozen=True)
class GameState:
    current_player: Player
    board_dimension: tuple[int, int]
    moves: list[str] = field (init=False, default_factory=list)

    class Props(TypedDict, total=False):
        current_player: Player

    def next_turn(self) -> Self:
        return self.to({
            'current_player': Player.PLAYER_1,
        }) if self.current_player == Player.PLAYER_2 \
        else self.to({
            'current_player': Player.PLAYER_2,
        })
    
    def to(self, props: Props) -> Self:
        ret = replace(self, **props)

        #if not ret.is_valid_state():
            #raise RuntimeError(f'Invalid game state: {ret}')

        return ret

class FeedbackInfo(StrEnum):
    VALID = 'Valid'
    NOT_CURRENT_PLAYER = 'Not current player'
    INVALID = 'Invalid'

@dataclass(frozen=True)
class Feedback:
    move: tuple[Location, Location]
    info: FeedbackInfo
