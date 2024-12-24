import pygame
from pygame import Rect, Surface
from typing import Any
import sys
#import random
#from cs150241project_networking import CS150241ProjectNetworking

class Board:
    _screen_width: int
    _screen_height: int
    _cell_length: int
    _grid: list[list[Rect]] = []
    _margin: int = 50

    def __init__(self, s_w: int, s_h: int, dim_x: int, dim_y: int):
        self._screen_width = s_w
        self._screen_height = s_h
        self._cell_length = min(
            (self._screen_width - (2 * self._margin)) // dim_x,
            (self._screen_height - (2 * self._margin)) // dim_y
        )
        self.create_grid(dim_x, dim_y)

    @property
    def cell_length(self) -> int:
        return self._cell_length

    def create_grid(self, row: int, col: int) -> None:
        for i in range(row):
            temp_row: list[Rect] = []
            for j in range(col):
                #length of cell will depend on which screen size is smaller
                length = min(
                    (self._screen_width - (2 * self._margin)) // row,
                    (self._screen_height - (2 * self._margin)) // col
                )

                #append cell to row
                temp_row.append(
                    pygame.Rect(
                        self.center_align(i, row, length, True),
                        self.center_align(j, col, length, False),
                        length,
                        length
                    )
                )
            #append row to grid
            self._grid.append(temp_row)
        return

    def center_align(self, num: int, dimension: int, length: int, is_x: bool) -> int:
        center_screen: int = self._screen_width // 2 if is_x else self._screen_height // 2
        center_index: int

        #odd center
        if dimension % 2 != 0:
            center_index = dimension // 2 + 1
            return (center_screen + (length // 2)) + (length * (num - center_index))
        #even center
        else:
            center_index = dimension // 2
            return (center_screen) + (length * (num - center_index))
    
    #event.pos is considered as Any type
    def snap_position(self, cursor_position: Any) -> Rect | None:
        for row in self._grid:
            for cell in row:
                if cell.collidepoint(cursor_position):
                    return cell
        return

    def render(self, screen: Surface):
        for ith,row in enumerate(self._grid):
            for jth,col in enumerate(row):
                # following chess.com format (LOL) 
                if (ith % 2 == 0 and jth % 2 == 0) or (ith % 2 != 0 and jth % 2 != 0):
                    pygame.draw.rect(screen, "lightyellow", col)
                else:
                    pygame.draw.rect(screen, "lightgreen", col)

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
        pieces: list[Rect] = []
        grid: Board = Board(self._width, self._height, 8, 8)

        is_running: bool = True
        #font = pygame.font.SysFont("", 24)

        #Rect: x, y, width, height
        targetBox: Rect = pygame.Rect(10, 10, grid.cell_length, grid.cell_length)
        pieces.append(targetBox)

        while is_running:
            screen.fill('black')
            
            grid.render(screen)

            for piece in pieces:
                pygame.draw.rect(screen, "chocolate1", piece)

            for event in pygame.event.get():
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1: #left mouse button
                        for index,piece in enumerate(pieces):
                            if piece.collidepoint(event.pos):
                                active_piece_index = index

                if event.type == pygame.MOUSEBUTTONUP:
                    #check which cell to snap to.
                    #note that the position should only snap to one cell (if not, we are in some big trouble)
                    snap_cell: Rect | None = grid.snap_position(event.pos)

                    if active_piece_index != None and snap_cell != None:
                        pieces[active_piece_index].clamp_ip(snap_cell)
                    
                    #reset active piece
                    active_piece_index = None

                if event.type == pygame.MOUSEMOTION:
                    if active_piece_index != None:
                        pieces[active_piece_index].move_ip(event.rel)

                if event.type == pygame.QUIT:
                    pygame.quit()
                    sys.exit()

            pygame.display.flip()

            clock.tick(fps)
            frame_count += 1

if __name__ == "__main__":
    view = BoardView(900, 900, 60)
    view.run()