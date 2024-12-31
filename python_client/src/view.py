from __future__ import annotations
import json
from math import ceil
import os
import pygame
from pygame import Rect, Surface, Clock, Font
import sys
from typing import Generator, NoReturn, Protocol
from cs150241project_networking import CS150241ProjectNetworking, Message
from project_types import (
    MakeMoveGameMessageContentDict,
    Player,
    PieceKind,
    Location,
    BoardGamePiecePositions,
    GameState,
    Feedback,
    FeedbackInfo,
    GameMessageType,
    GameMessageDict,
)
# import random

# --- MARK: Constants
REL_CAPTURED_BOX_WIDTH = 0.15
SCREEN_WIDTH = 1000
SCREEN_HEIGHT = 700
FPS = 60


# --- MARK: Position
# Position class is determining where an object is in the screen
class Position:
    _x: int
    _y: int

    @classmethod
    def from_tuple(cls, tup: tuple[int, int]) -> Position:
        return cls(tup[0], tup[1])

    def __init__(self, x: int, y: int) -> None:
        self._x = x
        self._y = y

    def __add__(self, other: Position | tuple[int, int]):
        if isinstance(other, Position):
            return Position(
                self.x + other.x,
                self.y + other.y
            )
        elif type(other) is tuple and list(map(type, other)) == [int, int]:
            return Position(
                self.x + other[0],
                self.y + other[1]
            )
        else:
            raise TypeError('Error: Invalid type for __add__ with Position()')

    def __iter__(self) -> Generator[int]:
        yield self._x
        yield self._y

    def __repr__(self) -> str:
        return f'{self._x, self._y}'

    def __rsub__(self, other: Position | tuple[int, int]):
        if isinstance(other, Position):
            return Position(
                other.x - self.x,
                other.y - self.y
            )
        elif type(other) is tuple and list(map(type, other)) == [int, int]:
            return Position(
                other[0] - self.x,
                other[1] - self.y
            )
        else:
            raise TypeError('Error: Invalid type for __rsub__ with Position()')

    def __sub__(self, other: Position | tuple[int, int]):
        if isinstance(other, Position):
            return Position(
                self.x - other.x,
                self.y - other.y
            )
        elif type(other) is tuple and list(map(type, other)) == [int, int]:
            return Position(
                self.x - other[0],
                self.y - other[1]
            )
        else:
            raise TypeError('Error: Invalid type for __sub__ with Position()')

    @property
    def x(self) -> int:
        return self._x

    @property
    def y(self) -> int:
        return self._y


# --- MARK: Piece
class Piece:
    _piece_kind: PieceKind
    _location: Location
    _position: Position
    _last_stable_position: Position
    _size: int
    _image: Surface
    _collision_box: Rect
    _owned_by: Player

    def __init__(
        self,
        piece_kind: PieceKind,
        location: Location,
        image: Surface,
        position: Position,
        size: int,
        owned_by: Player,
    ):
        self._piece_kind = piece_kind
        self._location = location
        self._position = position
        self._last_stable_position = self._position
        self._size = size
        self._image = pygame.transform.scale(image, (size, size))
        self._collision_box = pygame.Rect(
            position.x,
            position.y,
            self._size,
            self._size,
        )
        self._owned_by = owned_by

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def location(self) -> Location:
        return self._location

    @property
    def position(self) -> Position:
        return self._position

    @property
    def last_stable_position(self) -> Position:
        return self._last_stable_position

    @property
    def image(self) -> Surface:
        return self._image

    @property
    def collision_box(self) -> Rect:
        return self._collision_box

    @property
    def owned_by(self) -> Player:
        return self._owned_by

    def render(self, screen: Surface) -> None:
        # Note: it is important to render the collision box BEFORE the image to make the collision box "invisible"
        pygame.draw.rect(screen, 'chocolate1', self._collision_box)
        screen.blit(self._image, (self._position.x, self._position.y))

    # move both image and collision box using the relative position argument
    def move_rel(self, rel_position: tuple[int, int]) -> None:
        self._collision_box.move_ip(rel_position)
        # update self._position
        self._position += rel_position

    def move_abs(self, abs_position: tuple[int, int]) -> None:
        self._collision_box.move_ip(abs_position)
        # overwrite self._position
        self._position = Position.from_tuple(abs_position)

    def snap(self, cell_to_snap: Rect) -> None:
        self.collision_box.clamp_ip(cell_to_snap)
        self._position = Position(cell_to_snap.x, cell_to_snap.y)
        # update new stable position
        self._last_stable_position = self._position

    # return piece to previous spot
    def reset_to_spot(self) -> None:
        self._collision_box.update(
            (self._last_stable_position.x, self._last_stable_position.y),
            (self._size, self._size),
        )
        self._position = self._last_stable_position


# --- MARK: Grid
class Grid:
    _relative_width: int
    _relative_height: int
    _dim_x: int
    _dim_y: int
    _cell_length: int
    _grid: list[list[Rect]]
    _player: Player
    _margin: int = 20

    def __init__(self, s_w: int, s_h: int, dim_x: int, dim_y: int, player: Player) -> None:
        self._relative_width = s_w - int(REL_CAPTURED_BOX_WIDTH * s_w)
        self._relative_height = s_h
        self._dim_x = dim_x
        self._dim_y = dim_y
        self._cell_length = min(
            (self._relative_height - (2 * self._margin)) // dim_x,
            (self._relative_width - (2 * self._margin)) // dim_y,
        )
        self._grid = []
        self._player = player

        self._create_grid(dim_x, dim_y)

    @property
    def cell_length(self) -> int:
        return self._cell_length

    def _create_grid(self, row: int, col: int) -> None:
        for i in range(row):
            temp_row: list[Rect] = []
            for j in range(col):
                # append cell to row
                temp_row.append(
                    pygame.Rect(
                        self.center_align(j, col, self._cell_length, True),
                        self.center_align(i, row, self._cell_length, False),
                        self._cell_length,
                        self._cell_length,
                    )
                )
                
            # append row to grid (dependent on Player ID)
            match self._player:
                case Player.PLAYER_1:
                    self._grid.insert(0, temp_row)
                case Player.PLAYER_2:
                    temp_row.reverse()
                    self._grid.append(temp_row)

    def center_align(self, num: int, dimension: int, length: int, is_x: bool) -> int:
        center_screen: int = (
            self._relative_width // 2 if is_x else self._relative_height // 2
        )
        center_index: int

        # odd center
        if dimension % 2 != 0:
            center_index = dimension // 2 + 1
            return (center_screen + (length // 2)) + (length * (num - center_index))
        # even center
        else:
            center_index = dimension // 2
            return (center_screen) + (length * (num - center_index))

    def render(self, screen: Surface) -> None:
        for ith, row in enumerate(self._grid):
            for jth, col in enumerate(row):
                # following chess.com format (LOL)
                if (ith % 2 == 0 and jth % 2 == 0) or (ith % 2 != 0 and jth % 2 != 0):
                    pygame.draw.rect(screen, 'lightyellow', col)
                else:
                    pygame.draw.rect(screen, 'lightgreen', col)

    # event.pos is considered as Any type
    def snap_position(self, cursor_position: tuple[int, int]) -> Rect | None:
        for row in self._grid:
            for cell in row:
                if cell.collidepoint(cursor_position):
                    return cell

    # --- Grid conversions

    # note: location is 1-indexed
    def get_cell_from_location(self, loc: Location) -> Rect:
        return self._grid[loc.row - 1][loc.column - 1]
    
    def get_location_from_position(self, pos: Position) -> Location:
        zero_zero_location: Rect = self._grid[0][0]
        match self._player:
            case Player.PLAYER_1:
                return Location(
                    ((zero_zero_location.y - pos.y) // self._cell_length) + 1,
                    ((pos.x - zero_zero_location.x) // self._cell_length) + 1,
                )
            case Player.PLAYER_2:
                # return location based on player_id (due to a board rotated 180 degrees)
                return Location(
                    ((pos.y - zero_zero_location.y) // self._cell_length) + 1,
                    ((zero_zero_location.x - pos.x) // self._cell_length) + 1,
                )
    
    def get_position_from_cell(self, cell: Rect) -> Position:
        return Position(cell.x, cell.y)
    
    # function composition
    def get_location_from_cell(self, cell: Rect) -> Location:
        return self.get_location_from_position(self.get_position_from_cell(cell))
    
    def get_position_from_location(self, loc: Location) -> Position:
        return self.get_position_from_cell(self.get_cell_from_location(loc))
    
    def get_cell_from_position(self, pos: Position) -> Rect:
        return self.get_cell_from_location(self.get_location_from_position(pos))


# --- MARK: Button
class Button:
    _s: str
    _position: Position
    _bg_c: str
    _txt_c: str
    _render: Surface
    _rect: Rect

    def __init__(
        self, s: str, position: Position, bg_color: str, txt_color: str, font: Font
    ) -> None:
        self._s = s
        self._position = position
        self._bg_c = bg_color
        self._txt_c = txt_color

        self._render = font.render(self._s, True, self._txt_c, self._bg_c)
        self._rect = self._render.get_rect()
        self._rect.center = tuple(self._position)

    @property
    def content(self) -> str:
        return self._s

    @property
    def collision_box(self) -> Rect:
        return self._rect

    def update_text(self, s: str) -> None:
        self._s = s

    def update_text_color(self, color: str) -> None:
        self._txt_c = color

    def update_bg(self, color: str) -> None:
        self._bg_c = color

    def render(self, screen: Surface, font: Font) -> None:
        self._render: Surface = font.render(self._s, True, self._txt_c, self._bg_c)
        screen.blit(self._render, self._rect)


# --- MARK: Captured Pieces
class CapturedPiece:
    _piece_kind: PieceKind
    _size: int
    _collision_box: Rect
    _position: Position
    _last_stable_position: Position
    _image: Surface

    def __init__(self, piece: PieceKind, img: Surface, size: int, position: Position):
        self._piece_kind = piece
        self._size = size
        self._collision_box = pygame.Rect(
            position.x,
            position.y,
            self._size,
            self._size,
        )

        self._position = position
        self._last_stable_position = position
        self._image = pygame.transform.scale(img, (size, size))

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def size(self) -> int:
        return self._size

    @property
    def collision_box(self) -> Rect:
        return self._collision_box

    def render(self, screen: Surface) -> None:
        pygame.draw.rect(screen, 'chocolate1', self._collision_box)
        screen.blit(self._image, tuple(self._position))

    # move both image and collision box using the relative position argument
    def move_rel(self, rel_position: tuple[int, int]) -> None:
        self._collision_box.move_ip(rel_position)
        self._position += rel_position

    def reset_to_spot(self) -> None:
        self._collision_box.update(
            tuple(self._last_stable_position),
            (self._size, self._size),
        )


# --- MARK: Capture Box
class CaptureBox:
    MAX_PIECES: int = 4
    _width: int
    _height: int
    _position: Position
    _container: Rect
    _capture_list: list[CapturedPiece]
    _buttons: list[Button]
    _slice: tuple[int, int]
    _page: int

    def __init__(self, font: Font) -> None:
        self._width = int(SCREEN_WIDTH * REL_CAPTURED_BOX_WIDTH)
        self._height = SCREEN_HEIGHT

        self._position = Position(SCREEN_WIDTH - self._width, 0)

        self._container = pygame.Rect(
            SCREEN_WIDTH - self._width,
            0,
            self._width,
            self._height,
        )

        self._capture_list = []
        self._slice = (0, CaptureBox.MAX_PIECES)  # the list shows at most 4 pieces
        self._page = 1

        self._buttons = [
            Button(
                '<< Prev',
                Position(
                    self._position.x + (self._width // 2),
                    self._height - int(self._height * 0.15),
                ),
                'sandybrown',
                'black',
                font,
            ),
            Button(
                'Next >>',
                Position(
                    self._position.x + (self._width // 2),
                    self._height - int(self._height * 0.1),
                ),
                'sandybrown',
                'black',
                font,
            ),
        ]

    @property
    def buttons(self) -> list[Button]:
        return self._buttons

    @property
    def capture_list(self) -> list[CapturedPiece]:
        return self._capture_list

    @property
    def shown_capture_list(self) -> list[CapturedPiece]:
        return self._capture_list[self._slice[0] : self._slice[1]]

    @property
    def start_of_slice(self) -> int:
        return self._slice[0]

    def render(self, screen: Surface, font: Font) -> None:
        pygame.draw.rect(screen, 'sandybrown', self._container)

        # render header
        header_render: Surface = font.render('Captures', True, 'black', 'sandybrown')
        header_rect: Rect = header_render.get_rect()
        header_rect.center = (
            self._position.x + (self._width // 2),
            self._position.y + 20,
        )
        screen.blit(header_render, header_rect)

        for piece in self._capture_list[self._slice[0] : self._slice[1]]:
            piece.render(screen)

        for button in self._buttons:
            button.render(screen, font)

        # render footer
        footer_render: Surface = font.render(
            f'Page {self._page} of {1 if len(self._capture_list) == 0 else ceil(len(self._capture_list) / 4)}',
            True,
            'black',
            'sandybrown',
        )
        footer_rect: Rect = footer_render.get_rect()
        footer_rect.center = (self._position.x + (self._width // 2), self._height - 20)
        screen.blit(footer_render, footer_rect)

    def add_captured_piece(self, pk: PieceKind, img_path: str) -> None:
        self._capture_list.append(
            CapturedPiece(
                pk,
                pygame.image.load(img_path),
                int(self._width * 0.5),
                Position(
                    self._position.x
                    + (self._width // 2)
                    - (int(self._width * 0.5) // 2),
                    int(
                        (self._height * 0.1)
                        + (len(self._capture_list) % 4) * self._height * 0.175
                    ),
                ),
            )
        )

    def move_piece(self, rel: tuple[int, int], index: int) -> None:
        self._capture_list[index].move_rel(rel)

    def go_to_page(self, button_index: int) -> None:
        # previous page
        if button_index == 0:
            if self._slice[0] == 0:
                # do nothing
                return
            else:
                # update slice
                self._slice = (self._slice[0] - 4, self._slice[1] - 4)
                self._page -= 1
        # next page
        else:
            if self._slice[1] >= len(self._capture_list):
                # do nothing
                return
            else:
                # update slice
                self._slice = (self._slice[0] + 4, self._slice[1] + 4)
                self._page += 1


# --- MARK: Observers
class MakeMoveObserver(Protocol):
    def on_make_move(self, old: Location, new: Location, player: Player): ...


class MakeNewPieceObserver(Protocol):
    def on_make_new_piece(self, piece_kind: PieceKind, dest: Location): ...


class ReceiveMessageObserver(Protocol):
    def on_receive_message(self, message: Message): ...


class GameStateObserver(Protocol):
    def on_state_change(self, state: GameState): ...


# --- MARK: BoardGameView
class BoardGameView:
    _width: int
    _height: int
    _fps: int
    _screen: Surface
    _font: Font
    _clock: Clock
    _frame_count: int
    _pieces: dict[Location, Piece]
    _grid: Grid
    _capture_box: CaptureBox
    _current_player: Player
    _player: Player

    _captureBox: Rect
    _active_cell_to_snap: Rect | None

    # observers
    _make_move_observers: list[MakeMoveObserver]
    _make_new_piece_observers: list[MakeNewPieceObserver]
    _receive_message_observers: list[ReceiveMessageObserver]

    def __init__(self, state: GameState) -> None:
        pygame.init()

        self._width = SCREEN_WIDTH
        self._height = SCREEN_HEIGHT
        self._fps = FPS
        self._screen = pygame.display.set_mode((self._width, self._height))
        self._font = pygame.font.SysFont('', 24)
        self._clock = pygame.time.Clock()
        self._frame_count = 0
        self._pieces = {}
        self._capture_box = CaptureBox(self._font)

        # todo: setup networking to confirm this works
        self._current_player = state.player_to_move
        self._player = state.player

        # grid initialization comes after player initialization
        self._grid = Grid(
            self._width,
            self._height,
            # todo: not sure if rows or columns should've gone first
            state.board.columns,
            state.board.rows,
            self._player,
        )
        self._setup_initial_positions()

        # create observers for controller
        self._make_move_observers = []
        self._make_new_piece_observers = []
        self._receive_message_observers = []

        self._active_cell_to_snap = None

    # todo: will have to fix this to follow OCP
    def _setup_initial_positions(self) -> None:
        init_pos = BoardGamePiecePositions()

        for location, player_piece_kind in init_pos.get_positions().items():
            self._pieces[location] = Piece(
                player_piece_kind[1],
                location,
                pygame.image.load(self._setup_image(player_piece_kind[1])),
                self._grid.get_position_from_location(location),
                self._grid.cell_length,
                player_piece_kind[0],
            )
    
    def _setup_image(self, pk: PieceKind) -> str:
        match pk:
            case PieceKind.PAWN:
                return os.path.join('src', 'assets', 'lui_sword.jpg')
            case PieceKind.KING:
                return os.path.join('src', 'assets', 'lui_wink_ed.jpg')
            case PieceKind.LANCE:
                return os.path.join('src', 'assets', 'lui_bright.jpg')

    # register move observer (usually from controller)
    def register_make_move_observer(self, observer: MakeMoveObserver) -> None:
        self._make_move_observers.append(observer)

    def register_make_new_piece_observer(self, observer: MakeNewPieceObserver) -> None:
        self._make_new_piece_observers.append(observer)

    def register_receive_message_observer(self, observer: ReceiveMessageObserver) -> None:
        self._receive_message_observers.append(observer)

    # --- MARK: Run PyGame
    def run(self, networking: CS150241ProjectNetworking | None) -> NoReturn:
        active_piece_index: Location | None = None
        active_capture_piece_index: int | None = None

        previous_message: Message | None = None
        latest_message: Message | None = None

        is_running: bool = True

        while is_running:
            if networking is not None:
                for m in networking.recv():
                    latest_message = m

                if latest_message is not None and previous_message != latest_message:
                    for observer in self._receive_message_observers:
                        observer.on_receive_message(latest_message)

                    previous_message = latest_message

            for event in pygame.event.get():
                match event.type:
                    case pygame.MOUSEBUTTONDOWN:
                        match event.button:
                            case 1:  # left mouse button
                                # check if click is for Board
                                for loc in self._pieces.keys():
                                    piece = self._pieces[loc]
                                    if (
                                        piece.collision_box.collidepoint(event.pos)
                                        and piece.owned_by == self._player
                                    ):
                                        active_piece_index = loc
                                        break

                                # check if click is for Capture List: prev and next button
                                for index_b, button in enumerate(
                                    self._capture_box.buttons
                                ):
                                    if button.collision_box.collidepoint(event.pos):
                                        self._capture_box.go_to_page(index_b)

                                # check if click is for Capture List
                                # Note: the loop will only iterate through the shown capture list (maximum of 4)
                                for index_c, cap_piece in enumerate(
                                    self._capture_box.shown_capture_list,
                                    self._capture_box.start_of_slice,
                                ):
                                    if cap_piece.collision_box.collidepoint(event.pos):
                                        active_capture_piece_index = index_c
                                        break

                            case 3:
                                if active_piece_index is not None:
                                    # print("should reset")
                                    for loc in self._pieces.keys():
                                        piece = self._pieces[loc]
                                        if active_piece_index == loc:
                                            piece.reset_to_spot()
                                            break
                                    # point active piece index to nothing
                                    active_piece_index = None

                                if active_capture_piece_index is not None:
                                    for index_c, cap_piece in enumerate(
                                        self._capture_box.capture_list
                                    ):
                                        if active_capture_piece_index == index_c:
                                            cap_piece.reset_to_spot()
                                            break
                                    active_capture_piece_index = None

                            case _:
                                pass

                    case pygame.MOUSEBUTTONUP:
                        # check which cell to snap to.
                        # note that the position should only snap to one cell (if not, we are in some big trouble)
                        match event.button:
                            case 1:
                                if active_piece_index is not None:
                                    piece: Piece = self._pieces[active_piece_index]
                                    old_cell_location: Location = (
                                        self._grid.get_location_from_position(
                                            piece.last_stable_position
                                        )
                                    )
                                    snap_cell: Rect | None = self._grid.snap_position(event.pos)
                                    

                                    # todo: validate move through model
                                    if snap_cell is None:
                                        piece.reset_to_spot()
                                    else:
                                        new_cell_location: Location = self._grid.get_location_from_cell(snap_cell)

                                        self._active_cell_to_snap = snap_cell
                                        self._make_move(
                                            old_cell_location,
                                            new_cell_location,
                                            networking,
                                        )
                                        

                                    # point active piece index to nothing
                                    self._active_cell_to_snap = None
                                    active_piece_index = None

                                if active_capture_piece_index is not None:
                                    cap_piece: CapturedPiece = (
                                        self._capture_box.capture_list[
                                            active_capture_piece_index
                                        ]
                                    )
                                    snap_cell: Rect | None = self._grid.snap_position(event.pos)

                                    # todo: validate move through model
                                    if snap_cell is None:
                                        cap_piece.reset_to_spot()
                                    else:
                                        new_cell_location: Location = self._grid.get_location_from_cell(snap_cell)
                                        self._active_cell_to_snap = snap_cell
                                        self._make_new_piece(
                                            cap_piece.piece_kind,
                                            new_cell_location,
                                            networking,
                                        )
                                        
                                    active_capture_piece_index = None

                            case _:
                                pass
                    case pygame.MOUSEMOTION:
                        if active_piece_index is not None:
                            self._pieces[active_piece_index].move_rel(event.rel)

                        if active_capture_piece_index is not None:
                            self._capture_box.move_piece(
                                event.rel, active_capture_piece_index
                            )

                    case pygame.QUIT:
                        pygame.quit()
                        sys.exit()

                    case _:
                        pass

            # render all assets
            self.render_frame()

            pygame.display.flip()

            self._clock.tick(self._fps)
            self._frame_count += 1

    def render_frame(self) -> None:
        self._screen.fill('black')

        self._grid.render(self._screen)

        for loc in self._pieces.keys():
            self._pieces[loc].render(self._screen)

        self._capture_box.render(self._screen, self._font)

    def _make_move(
        self, old: Location, new: Location, networking: CS150241ProjectNetworking | None
    ) -> None:
        for observer in self._make_move_observers:
            observer.on_make_move(old, new, self._player)

        if networking is not None:
            # todo: implement move piece message
            message_content: MakeMoveGameMessageContentDict = {
                'src': {'row': old.row, 'column': old.column},
                'dest': {'row': new.row, 'column': new.column},
                'player': self._player,
            }

            data: GameMessageDict = {
                'frame': self._frame_count,
                'message_type': GameMessageType.MOVE,
                'message_content': message_content,
            }
            networking.send(json.dumps(data))

    def _make_new_piece(
        self,
        piece_kind: PieceKind,
        dest: Location,
        networking: CS150241ProjectNetworking | None,
    ) -> None:
        for observer in self._make_new_piece_observers:
            observer.on_make_new_piece(piece_kind, dest)

        if networking is not None:
            # todo: implement make new piece message
            networking.send(f'frame {self._frame_count} sent: make new piece')

    def update_move(self, fb: Feedback) -> None:
        active_piece: Piece = self._pieces[fb.move_src]
        match fb.info:
            case FeedbackInfo.VALID:
                if fb.move_dest is None:
                    raise RuntimeError('Error: Move destination was not found')

                self._active_cell_to_snap = self._grid.get_cell_from_location(
                    fb.move_dest
                )

                # remove piece from prev location
                self._pieces.pop(fb.move_src)

                # set piece to new location
                self._pieces[fb.move_dest] = active_piece

                # snap to location
                # if self._active_cell_to_snap is not None:
                self._pieces[fb.move_dest].snap(self._active_cell_to_snap)

            case _:
                self._pieces[fb.move_src].reset_to_spot()

    def update_new_piece(self, fb: Feedback) -> None:
        pass

    def on_state_change(self, state: GameState) -> None:
        self._player = state.player_to_move
