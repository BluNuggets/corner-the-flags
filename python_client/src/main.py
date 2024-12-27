# from cs150241project_networking import CS150241ProjectNetworking
from model import BoardGameModel
from view import BoardGameView, GameStateObserver
from project_types import (
    Location,
    GameState,
    Player,
    Feedback,
    FeedbackInfo
)


# todo: rename BoardGameController with actual board game name
class BoardGameController:
    _model: BoardGameModel
    _view: BoardGameView
    _game_state_observers: list[GameStateObserver]

    def __init__(self, model: BoardGameModel, view: BoardGameView) -> None:
        self._model = model
        self._view = view
        self._game_state_observers = []

    def start(self) -> None:
        view = self._view
        self._game_state_observers.append(view)
        view.register_make_move_observer(self)
        print("temporary print - controller.start()")

        view.run()

    def on_make_move(self, old: Location, new: Location, player: Player):
        feedback: Feedback = self._model.move_from_view(old, new, player)
        self._on_state_change(self._model.state)
        print(f"model says that the move is {"Valid" if feedback.info == FeedbackInfo.VALID else "Invalid"}")
        self._view.update_move(feedback)

    def _on_state_change(self, state: GameState):
        for observer in self._game_state_observers:
            observer.on_state_change(state)
        


def main() -> None:
    # networking: CS150241ProjectNetworking = CS150241ProjectNetworking.connect("localhost", 15000)
    model: BoardGameModel = BoardGameModel.default_board()
    view: BoardGameView = BoardGameView(model.state, 1)
    controller: BoardGameController = BoardGameController(model, view)

    controller.start()


if __name__ == "__main__":
    main()
