import pygame
from math import ceil
from pygame import Rect, Surface, Clock, Font
from typing import Any, Protocol
from project_types import (
    Player,
    PieceKind,
    Location,
    BoardGamePiecePositions,
    GameState,
    PieceData,
    Feedback,
    FeedbackInfo,
)
import sys
import os
# import random
# from cs150241project_networking import CS150241ProjectNetworking

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

    def __init__(self, x: int, y: int):
        self._x = x
        self._y = y

    def __repr__(self):
        return f"{self._x, self._y}"

    @property
    def x(self):
        return self._x

    @property
    def y(self):
        return self._y


# --- MARK: Piece
class Piece(PieceData):
    _piece_kind: PieceKind
    _location: Location
    _position: Position
    _image: Surface
    _collision_box: Rect
    _last_stable_position: Position
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
    def position(self):
        return self._position

    @property
    def last_stable_position(self):
        return self._last_stable_position

    @property
    def image(self):
        return self._image

    @property
    def collision_box(self):
        return self._collision_box

    @property
    def owned_by(self):
        return self._owned_by

    def render(self, screen: Surface):
        # Note: it is important to render the collision box BEFORE the image to make the collision box "invisible"
        pygame.draw.rect(screen, "chocolate1", self._collision_box)
        screen.blit(self._image, (self._position.x, self._position.y))

    # move both image and collision box using the relative position argument
    def move_rel(self, rel_position: Any):
        self._collision_box.move_ip(rel_position)

        # update self._position
        # todo: position type should support basic arithmetic
        self._position = Position(
            self._position.x + rel_position[0], self._position.y + rel_position[1]
        )

    def move_abs(self, abs_position: Any):
        self._collision_box.move_ip(abs_position)
        self._position = abs_position

    def snap(self, cell_to_snap: Rect):
        self.collision_box.clamp_ip(cell_to_snap)
        self._position = Position(cell_to_snap.x, cell_to_snap.y)

        # update new stable position
        self._last_stable_position = self._position

    # return piece to previous spot
    def reset_to_spot(self):
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
    _grid: list[list[Rect]] = []
    _player: Player
    _margin: int = 20

    def __init__(self, s_w: int, s_h: int, dim_x: int, dim_y: int, player: Player):
        self._relative_width = s_w - int(REL_CAPTURED_BOX_WIDTH * s_w)
        self._relative_height = s_h
        self._cell_length = min(
            (self._relative_height - (2 * self._margin)) // dim_x,
            (self._relative_width - (2 * self._margin)) // dim_y,
        )
        self._dim_x = dim_x
        self._dim_y = dim_y
        self._player = player
        self.create_grid(dim_x, dim_y)

    @property
    def cell_length(self) -> int:
        return self._cell_length

    def create_grid(self, row: int, col: int) -> None:
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
            self._grid.append(
                temp_row
            ) if self._player == Player.PLAYER_2 else self._grid.insert(0, temp_row)
        return

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

    def render(self, screen: Surface):
        for ith, row in enumerate(self._grid):
            for jth, col in enumerate(row):
                # following chess.com format (LOL)
                if (ith % 2 == 0 and jth % 2 == 0) or (ith % 2 != 0 and jth % 2 != 0):
                    pygame.draw.rect(screen, "lightyellow", col)
                else:
                    pygame.draw.rect(screen, "lightgreen", col)

    # event.pos is considered as Any type
    def snap_position(self, cursor_position: Any) -> Rect | None:
        for row in self._grid:
            for cell in row:
                if cell.collidepoint(cursor_position):
                    return cell
        return

    # note: location is 1-indexed
    def location_to_position(self, loc: Location) -> Position:
        return Position(
            self._grid[loc.row - 1][loc.column - 1].x,
            self._grid[loc.row - 1][loc.column - 1].y,
        )

    def position_to_location(self, pos: Position) -> Location:
        zero_zero_location = (self._grid[0][0].x, self._grid[0][0].y)
        # return location based on player_id (due to a flipped board)
        return (
            Location(
                ((pos.y - zero_zero_location[1]) // self._cell_length) + 1,
                ((pos.x - zero_zero_location[0]) // self._cell_length) + 1,
            )
            if self._player == Player.PLAYER_2
            else Location(
                ((zero_zero_location[1] - pos.y) // self._cell_length) + 1,
                ((pos.x - zero_zero_location[0]) // self._cell_length) + 1,
            )
        )

    def get_cell_location(self, cell: Rect | None) -> Location | None:
        if cell is None:
            return None
        else:
            return self.position_to_location(Position(cell.x, cell.y))


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
    ):
        self._s = s
        self._position = position
        self._bg_c = bg_color
        self._txt_c = txt_color

        self._render = font.render(self._s, True, self._txt_c, self._bg_c)
        self._rect = self._render.get_rect()
        self._rect.center = (self._position.x, self._position.y)

    @property
    def content(self) -> str:
        return self._s

    @property
    def collision_box(self) -> Rect:
        return self._rect

    def update_text(self, s: str):
        self._s = s

    def update_text_color(self, color: str):
        self._txt_c = color

    def update_bg(self, color: str):
        self._bg_c = color

    def render(self, screen: Surface, font: Font):
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

    def render(self, screen: Surface):
        pygame.draw.rect(screen, "chocolate1", self._collision_box)
        screen.blit(self._image, (self._position.x, self._position.y))

    # move both image and collision box using the relative position argument
    def move_rel(self, rel_position: Any):
        self._collision_box.move_ip(rel_position)
        self._position = Position(
            self._position.x + rel_position[0], self._position.y + rel_position[1]
        )

    def reset_to_spot(self):
        self._collision_box.update(
            (self._last_stable_position.x, self._last_stable_position.y),
            (self._size, self._size),
        )
        self._position = self._last_stable_position


# --- MARK: Capture Box
class CaptureBox:
    _width: int
    _height: int
    _position: Position
    _container: Rect
    _capture_list: list[CapturedPiece]
    _buttons: list[Button]
    _slice: tuple[int, int]
    _page: int

    def __init__(self, font: Font):
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
        self._slice = (0, 4)  # the list shows at most 4 pieces
        self._page = 1

        self._buttons = [
            Button(
                "<< Prev",
                Position(
                    self._position.x + (self._width // 2),
                    self._height - int(self._height * 0.15),
                ),
                "sandybrown",
                "black",
                font,
            ),
            Button(
                "Next >>",
                Position(
                    self._position.x + (self._width // 2),
                    self._height - int(self._height * 0.1),
                ),
                "sandybrown",
                "black",
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

    def render(self, screen: Surface, font: Font):
        pygame.draw.rect(screen, "sandybrown", self._container)

        # render header
        header_render: Surface = font.render("Captures", True, "black", "sandybrown")
        header_rect = header_render.get_rect()
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
            f"Page {self._page} of {1 if len(self._capture_list) == 0 else ceil(len(self._capture_list) / 4)}",
            True,
            "black",
            "sandybrown",
        )
        footer_rect = footer_render.get_rect()
        footer_rect.center = (self._position.x + (self._width // 2), self._height - 20)
        screen.blit(footer_render, footer_rect)

    def add_captured_piece(self, pk: PieceKind, img_path: str):
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

    def move_piece(self, rel: Any, index: int):
        self._capture_list[index].move_rel(rel)

    def go_to_page(self, button_index: int):
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

    def __init__(self, state: GameState, player: int):
        pygame.init()

        self._width = SCREEN_WIDTH
        self._height = SCREEN_HEIGHT
        self._fps = FPS
        self._screen = pygame.display.set_mode((self._width, self._height))
        self._font = pygame.font.SysFont("", 24)
        self._clock = pygame.time.Clock()
        self._frame_count = 0
        self._pieces = {}
        self._capture_box = CaptureBox(self._font)

        # todo: setup networking to confirm this works
        self._current_player = state.player_to_move
        self._player = Player.PLAYER_1 if player == 1 else Player.PLAYER_2

        # grid initialization comes after player initialization
        self._grid = Grid(
            self._width,
            self._height,
            # todo: not sure if rows or columns should've gone first
            state.board.rows,
            state.board.columns,
            self._player,
        )
        self._setup_initial_positions()

        # create observers for controller
        self._make_move_observers: list[MakeMoveObserver] = []
        self._make_new_piece_observers: list[MakeNewPieceObserver] = []

        self._active_cell_to_snap = None

    # will have to fix this to follow OCP
    def _setup_initial_positions(self):
        init_pos = BoardGamePiecePositions()

        for location, player_piece_kind in init_pos.get_positions().items():
            self._pieces[location] = Piece(
                player_piece_kind[1],
                location,
                pygame.image.load(self._setup_image(player_piece_kind[1])),
                self._grid.location_to_position(location),
                self._grid.cell_length,
                player_piece_kind[0],
            )

    def _setup_image(self, pk: PieceKind) -> str:
        match pk:
            case PieceKind.PAWN:
                return os.path.join("src", "assets", "lui_sword.jpg")
            case PieceKind.KING:
                return os.path.join("src", "assets", "lui_wink_ed.jpg")
            case PieceKind.LANCE:
                return os.path.join("src", "assets", "lui_bright.jpg")

    # register move observer (usually from controller)
    def register_make_move_observer(self, observer: MakeMoveObserver):
        self._make_move_observers.append(observer)

    def register_make_new_piece_observer(self, observer: MakeNewPieceObserver):
        self._make_new_piece_observers.append(observer)

    # --- MARK: Run PyGame
    def run(self):
        active_piece_index: Location | None = None
        active_capture_piece_index: int | None = None

        is_running: bool = True

        while is_running:
            for event in pygame.event.get():
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1:  # left mouse button
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
                        for index_b, button in enumerate(self._capture_box.buttons):
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

                    if event.button == 3:
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

                if event.type == pygame.MOUSEBUTTONUP:
                    # check which cell to snap to.
                    # note that the position should only snap to one cell (if not, we are in some big trouble)
                    if event.button == 1:
                        if active_piece_index is not None:
                            piece: Piece = self._pieces[active_piece_index]
                            old_cell_location: Location = (
                                self._grid.position_to_location(
                                    piece.last_stable_position
                                )
                            )
                            snap_cell: Rect | None = self._grid.snap_position(event.pos)
                            new_cell_location: Location | None = (
                                self._grid.get_cell_location(snap_cell)
                            )

                            # todo: validate move through model
                            if snap_cell is not None and new_cell_location is not None:
                                self._active_cell_to_snap = snap_cell
                                self._make_move(old_cell_location, new_cell_location)
                            else:
                                piece.reset_to_spot()

                            # point active piece index to nothing
                            self._active_cell_to_snap = None
                            active_piece_index = None

                        if active_capture_piece_index is not None:
                            cap_piece: CapturedPiece = self._capture_box.capture_list[
                                active_capture_piece_index
                            ]
                            snap_cell: Rect | None = self._grid.snap_position(event.pos)
                            new_cell_location: Location | None = (
                                self._grid.get_cell_location(snap_cell)
                            )

                            # todo: validate move through model
                            if snap_cell is not None and new_cell_location is not None:
                                self._active_cell_to_snap = snap_cell
                                self._make_new_piece(
                                    cap_piece.piece_kind, new_cell_location
                                )
                            else:
                                cap_piece.reset_to_spot()

                            active_capture_piece_index = None

                if event.type == pygame.MOUSEMOTION:
                    if active_piece_index is not None:
                        self._pieces[active_piece_index].move_rel(event.rel)

                    if active_capture_piece_index is not None:
                        self._capture_box.move_piece(
                            event.rel, active_capture_piece_index
                        )

                if event.type == pygame.QUIT:
                    pygame.quit()
                    sys.exit()

            # render all assets
            self.render_frame()

            pygame.display.flip()

            self._clock.tick(self._fps)
            self._frame_count += 1

    def render_frame(self):
        self._screen.fill("black")

        self._grid.render(self._screen)

        for loc in self._pieces.keys():
            self._pieces[loc].render(self._screen)

        self._capture_box.render(self._screen, self._font)

    def _make_move(self, old: Location, new: Location):
        for observer in self._make_move_observers:
            observer.on_make_move(old, new, self._player)

    def _make_new_piece(self, piece_kind: PieceKind, dest: Location):
        for observer in self._make_new_piece_observers:
            observer.on_make_new_piece(piece_kind, dest)

    def update_move(self, fb: Feedback):
        active_piece = self._pieces[fb.move[0]]
        match fb.info:
            case FeedbackInfo.VALID:
                # remove piece from prev location
                self._pieces.pop(fb.move[0])
                # set piece to new location
                self._pieces[fb.move[1]] = active_piece

                # snap to location
                if self._active_cell_to_snap is not None:
                    self._pieces[fb.move[1]].snap(self._active_cell_to_snap)

            case FeedbackInfo.NOT_CURRENT_PLAYER:
                self._pieces[fb.move[0]].reset_to_spot()

            case FeedbackInfo.INVALID:
                self._pieces[fb.move[0]].reset_to_spot()

    def update_new_piece(self, fb: Feedback):
        pass

    def on_state_change(self, state: GameState):
        self._player = state.player_to_move
