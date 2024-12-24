import pygame
from pygame import Rect, Surface
from typing import Any
import sys
import os
#import random
#from cs150241project_networking import CS150241ProjectNetworking

REL_CAPTURED_BOX_WIDTH = 0.15

class Board:
    _relative_width: int
    _relative_height: int
    _cell_length: int
    _grid: list[list[Rect]] = []
    _margin: int = 20

    def __init__(self, s_w: int, s_h: int, dim_x: int, dim_y: int):
        self._relative_width = s_w - int(REL_CAPTURED_BOX_WIDTH * s_w)
        self._relative_height = s_h
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

#todo: protocol for pieces
#todo: Position type should be implemented
class Pawn():
    _position: tuple[int, int]
    _image: Surface
    _collision_box: Rect

    def __init__(self, image: Surface, x: int, y: int, size: int):
        self._position = (x, y)
        self._size = size
        self._image = pygame.transform.scale(image, (size, size))
        self._collision_box = pygame.Rect(
            self._position[0],
            self._position[1],
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
        screen.blit(self._image, self._position)

    #move both image and collision box using the relative position argument
    def move(self, screen: Surface, rel_position: Any):
        self._collision_box.move_ip(rel_position)
        
        #update self._position
        #todo: position type should support basic arithmetic
        self._position = (self._position[0] + rel_position[0], self._position[1] + rel_position[1])
        screen.blit(self._image, self._position)

    def snap(self, cell_to_snap: Rect, screen: Surface):
        self.collision_box.clamp_ip(cell_to_snap)

        #update self._position
        self._position = (self._collision_box.x,self._collision_box.y)
        screen.blit(self._image, self._position)


#todo: add screen, fps, frame_count and clock as variables outside run function
class BoardView:
    _width: int
    _height: int
    _fps: int

    def __init__(self, width: int, height: int, fps: int):
        self._width = width
        self._height = height
        self._fps = fps

    def run(self):
        pygame.init()

        screen = pygame.display.set_mode((self._width, self._height))
        clock = pygame.time.Clock()
        fps = self._fps
        frame_count = 0

        active_piece_index: int | None = None
        pieces: list[Pawn] = []
        grid: Board = Board(self._width, self._height, 8, 8)

        is_running: bool = True
        #font = pygame.font.SysFont("", 24)

        #Rect: x, y, width, height
        captureBox: Rect = pygame.Rect(
            self._width - (REL_CAPTURED_BOX_WIDTH * self._width),
            0, 
            self._width, 
            self._height,
        )

        looi: Pawn = Pawn(pygame.image.load(os.path.join("src", "assets", "lui_sword.jpg")), 10, 10, grid.cell_length)
        pieces.append(looi)

        while is_running:
            for event in pygame.event.get():
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1: #left mouse button
                        for index,piece in enumerate(pieces):
                            if piece.collision_box.collidepoint(event.pos):
                                print("active box hit")
                                active_piece_index = index

                if event.type == pygame.MOUSEBUTTONUP:
                    #check which cell to snap to.
                    #note that the position should only snap to one cell (if not, we are in some big trouble)
                    snap_cell: Rect | None = grid.snap_position(event.pos)

                    if active_piece_index != None and snap_cell != None:
                        pieces[active_piece_index].snap(snap_cell, screen)
                    
                    #point active piece index to nothing
                    active_piece_index = None

                if event.type == pygame.MOUSEMOTION:
                    if active_piece_index != None:
                        print(event.rel)
                        pieces[active_piece_index].move(screen, event.rel)

                if event.type == pygame.QUIT:
                    pygame.quit()
                    sys.exit()

            #render all assets
            screen.fill('black')
            
            grid.render(screen)

            #captured box (WIP)
            pygame.draw.rect(screen, "sandybrown", captureBox)

            for piece in pieces:
                piece.render(screen)

            pygame.display.flip()

            clock.tick(fps)
            frame_count += 1

if __name__ == "__main__":
    view = BoardView(900, 700, 60)
    view.run()