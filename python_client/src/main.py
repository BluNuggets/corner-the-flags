# from cs150241project_networking import CS150241ProjectNetworking
from model import BoardGameModel, Board
from view import BoardGameView


# todo: rename BoardGameController with actual board game name
class BoardGameController:
    _model: BoardGameModel
    _view: BoardGameView

    def __init__(self, model: BoardGameModel, view: BoardGameView) -> None:
        self._model = model
        self._view = view

    def start(self) -> None:
        model = self._model
        view = self._view

        print("temporary print - controller.start()")

        view.run()
        


def main() -> None:
    # networking: CS150241ProjectNetworking = CS150241ProjectNetworking.connect("localhost", 15000)
    model: BoardGameModel = BoardGameModel(Board(8, 8))
    view: BoardGameView = BoardGameView(900, 700, 60, 8, 8)
    controller: BoardGameController = BoardGameController(model, view)

    controller.start()


if __name__ == "__main__":
    main()
