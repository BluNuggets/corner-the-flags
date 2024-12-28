from cs150241project_networking import CS150241ProjectNetworking
from cs150241project_networking.main import PlayerId
from model import BoardGameModel


# todo: rename BoardGameController with actual board game name
class BoardGameController:
    _model: BoardGameModel
    # _view: BoardGameView

    def __init__(self, model: BoardGameModel) -> None:
        self._model = model
        # self._view = view

    def start(self) -> None:
        print("temporary print - controller.start()")


def main() -> None:
    networking: CS150241ProjectNetworking = CS150241ProjectNetworking.connect(
        "localhost", 15000
    )
    player_id: PlayerId = networking.player_id if PlayerId else PlayerId(1)

    model: BoardGameModel = BoardGameModel.setup_game(player_id)
    controller: BoardGameController = BoardGameController(model)
    controller.start()


if __name__ == "__main__":
    main()
