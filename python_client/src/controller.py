from cs150241project_networking import CS150241ProjectNetworking, Message
from model import BoardGameModel
from view import BoardGameView, GameStateObserver
from project_types import (
    Location,
    GameState,
    Player,
    PieceKind,
    Feedback,
)


# todo: rename BoardGameController with actual board game name
class BoardGameController:
    _model: BoardGameModel
    _view: BoardGameView
    _networking: CS150241ProjectNetworking | None
    _game_state_observers: list[GameStateObserver]

    def __init__(
        self,
        model: BoardGameModel,
        view: BoardGameView,
        networking: CS150241ProjectNetworking | None,
    ) -> None:
        self._model = model
        self._view = view
        self._networking = networking
        self._game_state_observers = []

    def start(self) -> None:
        view = self._view
        self._game_state_observers.append(view)
        view.register_make_move_observer(self)
        view.register_receive_message_observer(self)
        print('temporary print - controller.start()')

        view.run(self._networking)

    def on_make_move(self, old: Location, new: Location, player: Player) -> None:
        feedback: Feedback = self._model.move_piece(old, new, player)
        self._on_state_change(self._model)
        # print(f"model says that the move is {"Valid" if feedback.info == FeedbackInfo.VALID else "Invalid"}")
        self._view.update_move(feedback)

    def on_make_new_piece(self, piece_kind: PieceKind, dest: Location) -> None:
        # feedback: Feedback = self._model.add_new_piece(piece_kind, dest)
        # self._on_state_change(self._model.state)
        # print(f"model says that the move is {"Valid" if feedback.info == FeedbackInfo.VALID else "Invalid"}")
        # self._view.update_new_piece(feedback)
        pass

    def on_receive_message(self, message: Message) -> None:
        if self._networking is not None:
            if self._networking.player_id != message.source:
                print(f'Message from Player {message.source}: {message.payload}')

    def _on_state_change(self, state: GameState) -> None:
        for observer in self._game_state_observers:
            observer.on_state_change(state)
