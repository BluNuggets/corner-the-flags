from cs150241project_networking import CS150241ProjectNetworking
from cs150241project_networking.main import PlayerId
from model import BoardGameModel
from view import BoardGameView
from controller import BoardGameController


def main() -> None:
    try:
        networking: CS150241ProjectNetworking | None = (
            CS150241ProjectNetworking.connect("localhost", 15000)
        )
    except Exception:
        networking = None

    player_id: PlayerId = networking.player_id if networking else PlayerId(1)

    model: BoardGameModel = BoardGameModel.setup_game(player_id)
    view: BoardGameView = BoardGameView(model, 1)
    controller: BoardGameController = BoardGameController(model, view)

    controller.start()


if __name__ == "__main__":
    main()
