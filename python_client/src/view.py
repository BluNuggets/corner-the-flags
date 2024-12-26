import pygame
from pygame import Rect, Surface, Clock
from typing import Any
from enum import StrEnum
import sys
import os
#import random
#from cs150241project_networking import CS150241ProjectNetworking

REL_CAPTURED_BOX_WIDTH = 0.15

class PieceKind(StrEnum):
    PAWN = 'Pawn'
    LANCE = 'Lance'

#Location class is determining where Piece is in the Grid
class Location():
    _row: int
    _col: int

    def __init__(self, col: int, row: int):
        self._row = row
        self._col = col

    @property
    def x(self):
        return self._row
    
    @property
    def y(self):
        return self._col

#Position class is determining where an object is in the screen
class Position():
    _x: int
    _y: int

    def __init__(self, x: int, y: int):
        self._x = x
        self._y = y

    @property
    def x(self):
        return self._x
    
    @property
    def y(self):
        return self._y
        
    
#todo: convert some variables to Position type
class Piece():
    # Note: position is only relative to the size of the screen
    # The piece does not know what location it is on the Board (i.e. A1, B3 are not known by Piece)
    _position: Position
    _image: Surface
    _collision_box: Rect
    _last_stable_position: Position

    def __init__(self, image: Surface, position: Position, size: int):
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
    def position(self):
        return self._position

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

class Pawn(Piece):
    def valid_move(self):
        pass

class Lance(Piece):
    def valid_move(self):
        pass

class Grid:
    _relative_width: int
    _relative_height: int
    _cell_length: int
    _center: Location
    _grid: list[list[Rect]] = []
    _margin: int = 20

    def __init__(self, s_w: int, s_h: int, dim_x: int, dim_y: int):
        self._relative_width = s_w - int(REL_CAPTURED_BOX_WIDTH * s_w)
        self._relative_height = s_h
        self.center = Location(self._relative_width // 2, self._relative_height // 2)
        self._cell_length = min(
            (self._relative_width - (2 * self._margin)) // dim_x,
            (self._relative_height - (2 * self._margin)) // dim_y
        )
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
                        self.center_align(i, row, self._cell_length, True),
                        self.center_align(j, col, self._cell_length, False),
                        self._cell_length,
                        self._cell_length
                    )
                )
            #append row to grid
            self._grid.append(temp_row)
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
    
    def location_to_position(self, loc: Location) -> Position:
        return Position(self._grid[loc.x][loc.y].x, self._grid[loc.x][loc.y].y)
    
    def move_piece(self, piece: Piece, new_raw_position: Any):
        snap_cell: Rect | None = self.snap_position(new_raw_position)

        piece.snap(snap_cell) if snap_cell != None else piece.reset_to_spot()
    
class BoardView:
    _width: int
    _height: int
    _fps: int
    _screen: Surface
    _clock: Clock
    _frame_count: int
    _pieces: list[Piece]
    _grid: Grid

    _captureBox: Rect

    def __init__(self, width: int, height: int, fps: int, initial_positions: list[tuple[Location, PieceKind]]):
        self._width = width
        self._height = height
        self._fps = fps
        self._screen = pygame.display.set_mode((self._width, self._height))
        self._clock = pygame.time.Clock()
        self._frame_count = 0
        self._pieces = []
        #todo: change dimensions based on initial positions here
        self._grid = Grid(self._width, self._height, 5, 5)

        self.setup_initial_positions(initial_positions)
    
    # will have to fix this to follow OCP
    def setup_initial_positions(self, init_pos: list[tuple[Location, PieceKind]]):
        for pos in init_pos:
            match pos[1]:
                case PieceKind.PAWN:
                    pawn: Pawn = Pawn(pygame.image.load(os.path.join("src", "assets", "lui_sword.jpg")), self._grid.location_to_position(pos[0]), self._grid.cell_length)
                    self._pieces.append(pawn)
                    pass
                case PieceKind.LANCE:
                    lance: Lance = Lance(pygame.image.load(os.path.join("src", "assets", "lui_wink_ed.jpg")), self._grid.location_to_position(pos[0]), self._grid.cell_length)
                    self._pieces.append(lance)

    def run(self):
        pygame.init()

        active_piece_index: int | None = None

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
                    print(event)
                    if event.button == 1: #left mouse button
                        for index,piece in enumerate(self._pieces):
                            if piece.collision_box.collidepoint(event.pos):                
                                # move piece to the end of list
                                self._pieces.pop(index)
                                self._pieces.append(piece)

                                active_piece_index = len(self._pieces) - 1
                                break
                    if event.button == 3 and active_piece_index != None:
                        print("should reset")
                        for index,piece in enumerate(self._pieces):
                            if active_piece_index == index:
                                piece.reset_to_spot()  
                            else:
                                continue
                        
                        #point active piece index to nothing
                        active_piece_index = None

                if event.type == pygame.MOUSEBUTTONUP:
                    #check which cell to snap to.
                    #note that the position should only snap to one cell (if not, we are in some big trouble)
                    if event.button == 1 and active_piece_index != None:
                        #move piece in the grid
                        self._grid.move_piece(self._pieces[active_piece_index], event.pos)
                        
                        #point active piece index to nothing
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

        for piece in self._pieces:
            piece.render(self._screen)


if __name__ == "__main__":
    init_pos: list[tuple[Location, PieceKind]] = [
            (Location(2, 1), PieceKind.PAWN),
            (Location(3, 0), PieceKind.LANCE),
            (Location(0, 2), PieceKind.LANCE),
            (Location(1, 1), PieceKind.PAWN),
        ]
    view = BoardView(900, 700, 60, init_pos)
    view.run()