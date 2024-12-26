from __future__ import annotations
from dataclasses import dataclass
from enum import StrEnum


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
