import pygame
from pygame import Rect, Surface, Clock
from typing import Any, Protocol
from project_types import (
    Player,
    PieceKind,
    Location,
    BoardGamePiecePositions,
    GameState,
    PieceData,
    Feedback,
    FeedbackInfo
)
import sys
import os
#import random
#from cs150241project_networking import CS150241ProjectNetworking

REL_CAPTURED_BOX_WIDTH = 0.15
SCREEN_WIDTH = 1000
SCREEN_HEIGHT = 700
FPS = 60

#Position class is determining where an object is in the screen
class Position():
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
    
class Piece(PieceData):
    # Note: position is only relative to the size of the screen
    # The piece does not know what location it is on the Board (i.e. A1, B3 are not known by Piece)
    _piece_kind: PieceKind
    _location: Location
    _position: Position
    _image: Surface
    _collision_box: Rect
    _last_stable_position: Position

    def __init__(self, piece_kind: PieceKind, location: Location, image: Surface, position: Position, size: int):
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

    def render(self, screen: Surface):
        #Note: it is important to render the collision box BEFORE the image to make the collision box "invisible"
        pygame.draw.rect(screen, "chocolate1", self._collision_box)
        screen.blit(self._image, (self._position.x, self._position.y))

    #move both image and collision box using the relative position argument
    def move_rel(self, rel_position: Any):
        self._collision_box.move_ip(rel_position)
        
        #update self._position
        #todo: position type should support basic arithmetic
        self._position = Position(self._position.x + rel_position[0], self._position.y + rel_position[1])

    def move_abs(self, abs_position: Any):
        self._collision_box.move_ip(abs_position)
        self._position = (abs_position)
        
    def snap(self, cell_to_snap: Rect):
        self.collision_box.clamp_ip(cell_to_snap)
        self._position = Position(cell_to_snap.x,cell_to_snap.y)

        #update new stable position
        self._last_stable_position = self._position

    #return piece to previous spot
    def reset_to_spot(self):
        self._collision_box.update((self._last_stable_position.x, self._last_stable_position.y), (self._size, self._size))
        self._position = self._last_stable_position

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
            (self._relative_width - (2 * self._margin)) // dim_y
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
                #append cell to row
                temp_row.append(
                    pygame.Rect(
                        self.center_align(j, col, self._cell_length, True),
                        self.center_align(i, row, self._cell_length, False),
                        self._cell_length,
                        self._cell_length
                    )
                )
            #append row to grid (dependent on Player ID)
            self._grid.append(temp_row) if self._player == Player.PLAYER_2 else self._grid.insert(0, temp_row)
        return

    def center_align(self, num: int, dimension: int, length: int, is_x: bool) -> int:
        center_screen: int = self._relative_width // 2 if is_x else self._relative_height // 2
        center_index: int

        #odd center
        if dimension % 2 != 0:
            center_index = dimension // 2 + 1
            return (center_screen + (length // 2)) + (length * (num - center_index))
        #even center
        else:
            center_index = dimension // 2
            return (center_screen) + (length * (num - center_index))
    
    def render(self, screen: Surface):
        for ith,row in enumerate(self._grid):
            for jth,col in enumerate(row):
                # following chess.com format (LOL)
                if (ith % 2 == 0 and jth % 2 == 0) or (ith % 2 != 0 and jth % 2 != 0):
                    pygame.draw.rect(screen, "lightyellow", col)
                else:
                    pygame.draw.rect(screen, "lightgreen", col)

    #event.pos is considered as Any type
    def snap_position(self, cursor_position: Any) -> Rect | None:
        for row in self._grid:
            for cell in row:
                if cell.collidepoint(cursor_position):
                    return cell
        return
    
    #note: location is 1-indexed
    def location_to_position(self, loc: Location) -> Position:
        return Position(
            self._grid[loc.row-1][loc.column-1].x,
            self._grid[loc.row-1][loc.column-1].y
        )
    
    def position_to_location(self, pos: Position) -> Location:
        zero_zero_location = (self._grid[0][0].x, self._grid[0][0].y)
        # return location based on player_id (due to a flipped board)
        return Location(
                ((pos.y - zero_zero_location[1]) // self._cell_length) + 1,
                ((pos.x - zero_zero_location[0]) // self._cell_length) + 1
            ) if self._player == Player.PLAYER_2 else \
            Location(
                ((zero_zero_location[1] - pos.y) // self._cell_length) + 1,
                ((pos.x - zero_zero_location[0]) // self._cell_length) + 1
            )

    def get_cell_location(self, cell: Rect | None) -> Location | None:
        if cell == None:
            return None
        else:
            return self.position_to_location(Position(cell.x, cell.y))

class MakeMoveObserver(Protocol):
    def on_make_move(self, old: Location, new: Location, player: Player):
        ...

class GameStateObserver(Protocol):
    def on_state_change(self, state: GameState):
        ...

class BoardGameView:
    _width: int
    _height: int
    _fps: int
    _screen: Surface
    _clock: Clock
    _frame_count: int
    _pieces: dict[Location, Piece]
    _grid: Grid
    _player: Player
    _nth_action: int

    _captureBox: Rect
    _active_cell_to_snap: Rect | None

    def __init__(self, state: GameState):
        self._width = SCREEN_WIDTH
        self._height = SCREEN_HEIGHT
        self._fps = FPS
        self._screen = pygame.display.set_mode((self._width, self._height))
        self._clock = pygame.time.Clock()
        self._frame_count = 0
        self._pieces = {}

        #todo: setup networking to confirm this works
        self._player = state.current_player

        #grid initialization comes after player initialization
        self._grid = Grid(self._width, self._height, state.board_dimension[0], state.board_dimension[1], self._player)
        self._setup_initial_positions()

        #create observers for controller
        self._make_move_observers: list[MakeMoveObserver] = []

        self._active_cell_to_snap = None

    # will have to fix this to follow OCP
    def _setup_initial_positions(self):
        init_pos = BoardGamePiecePositions()

        for (location, player_piece_kind) in init_pos.get_positions().items():
            self._pieces[location] = Piece(
                player_piece_kind[1],
                location,
                self._setup_image(player_piece_kind[1]),
                self._grid.location_to_position(location),
                self._grid.cell_length
            )

    def _setup_image(self, pk: PieceKind) -> Surface:
        match pk:
            case PieceKind.PAWN:
                return pygame.image.load(os.path.join("src", "assets", "lui_sword.jpg"))
            case PieceKind.KING:
                return pygame.image.load(os.path.join("src", "assets", "lui_wink_ed.jpg"))
            case PieceKind.LANCE:
                return pygame.image.load(os.path.join("src", "assets", "lui_bright.jpg"))


    # register move observer (usually from controller)
    def register_make_move_observer(self, observer: MakeMoveObserver):
        self._make_move_observers.append(observer)

    def run(self):
        pygame.init()

        active_piece_index: Location | None = None

        is_running: bool = True
        #font = pygame.font.SysFont("", 24)

        #Rect: x, y, width, height
        self._captureBox: Rect = pygame.Rect(
            self._width - (REL_CAPTURED_BOX_WIDTH * self._width),
            0, 
            self._width, 
            self._height,
        )

        while is_running:
            for event in pygame.event.get():
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1: #left mouse button
                        for loc in self._pieces.keys():
                            piece = self._pieces[loc]
                            if piece.collision_box.collidepoint(event.pos):                
                                active_piece_index = loc
                                break

                    if event.button == 3 and active_piece_index != None:
                        # print("should reset")
                        for loc in self._pieces.keys():
                            piece = self._pieces[loc]
                            if active_piece_index == loc:
                                piece.reset_to_spot()  
                            else:
                                continue
                        
                        #point active piece index to nothing
                        active_piece_index = None

                if event.type == pygame.MOUSEBUTTONUP:
                    #check which cell to snap to.
                    #note that the position should only snap to one cell (if not, we are in some big trouble)
                    if event.button == 1 and active_piece_index != None:
                        piece: Piece = self._pieces[active_piece_index]
                        old_cell_location: Location = self._grid.position_to_location(piece.last_stable_position)
                        snap_cell: Rect | None = self._grid.snap_position(event.pos)
                        new_cell_location: Location | None = self._grid.get_cell_location(snap_cell)

                        #todo: validate move through model
                        if snap_cell != None and new_cell_location != None:
                            self._active_cell_to_snap = snap_cell
                            self._make_move(old_cell_location, new_cell_location)
                        else:
                            piece.reset_to_spot()
                        
                        #point active piece index to nothing
                        self._active_cell_to_snap = None
                        active_piece_index = None

                if event.type == pygame.MOUSEMOTION:
                    if active_piece_index != None:
                        self._pieces[active_piece_index].move_rel(event.rel)

                if event.type == pygame.QUIT:
                    pygame.quit()
                    sys.exit()

            #render all assets
            self.render_frame()

            pygame.display.flip()

            self._clock.tick(self._fps)
            self._frame_count += 1

    def render_frame(self):
        self._screen.fill('black')
            
        self._grid.render(self._screen)

        #todo: captured box
        pygame.draw.rect(self._screen, "sandybrown", self._captureBox)

        for loc in self._pieces.keys():
            self._pieces[loc].render(self._screen)            
        
    def _make_move(self, old: Location, new: Location):
        for observer in self._make_move_observers:
            observer.on_make_move(old, new, self._player)

    def update_move(self, fb: Feedback):
        active_piece = self._pieces[fb.move[0]]
        match fb.info:
            case FeedbackInfo.VALID:
                #remove piece from prev location
                self._pieces.pop(fb.move[0])
                #set piece to new location
                self._pieces[fb.move[1]] = active_piece

                #snap to location
                if self._active_cell_to_snap != None:
                    self._pieces[fb.move[1]].snap(self._active_cell_to_snap)

            case FeedbackInfo.NOT_CURRENT_PLAYER:
                self._pieces[fb.move[0]].reset_to_spot()

            case FeedbackInfo.INVALID:
                self._pieces[fb.move[0]].reset_to_spot()

    def on_state_change(self, state: GameState):
        self._player = state.current_player