from __future__ import annotations
from dataclasses import dataclass, field
import json
from cs150241project_networking import CS150241ProjectNetworking, Message
from model import BoardGameModel
from view import BoardGameView, GameStateObserver
from project_types import (
    Location,
    GameState,
    LocationDict,
    MakeMoveGameMessageContentDict,
    Player,
    PieceKind,
    MoveFeedback,
    MoveFeedbackInfo,
    GameMessageType,
    GameMessageDict,
)

# --- MARK: BoardGameControler


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
        view: BoardGameView = self._view
        self._game_state_observers.append(view)
        view.register_make_move_observer(self)
        view.register_receive_message_observer(self)
        print('temporary print - controller.start()')

        view.run(self._networking)

    def on_make_move(self, old: Location, new: Location, player: Player) -> None:
        feedback: MoveFeedback = self._model.move_piece(old, new, player)
        self._on_state_change(self._model)
        print(
            f"model says that the move is {"Valid" if feedback.info == MoveFeedbackInfo.VALID else "Invalid"}"
        )
        self._view.update_move(feedback)

    def on_make_new_piece(self, piece_kind: PieceKind, dest: Location) -> None:
        # feedback: MoveFeedback = self._model.add_new_piece(piece_kind, dest)
        # self._on_state_change(self._model.state)
        # print(f"model says that the move is {"Valid" if feedback.info == MoveFeedbackInfo.VALID else "Invalid"}")
        # self._view.update_new_piece(feedback)
        pass

    def on_receive_message(self, message: Message) -> None:
        if self._networking is not None:
            if self._networking.player_id != message.source:
                print(f'Message from Player {message.source}: {message.payload}')
                game_message: GameMessage = GameMessageFactory.make(message.payload)

                message_type: GameMessageType = game_message.message_type
                message_content: GameMessageContent = game_message.message_content
                print(message_type)
                print(message_content)
                # todo: figure out how to apply OCP (subtype polymorphism) for converting strings to Python objects
                match GameMessageType(message_type):
                    case GameMessageType.MOVE:
                        if message_content.move_src is None:
                            return

                        if message_content.move_dest is None:
                            return

                        if message_content.player is None:
                            return

                        self.on_make_move(
                            message_content.move_src,
                            message_content.move_dest,
                            message_content.player,
                        )
                    case GameMessageType.INVALID:
                        pass

    def _on_state_change(self, state: GameState) -> None:
        for observer in self._game_state_observers:
            observer.on_state_change(state)


# --- MARK: GameMessage


@dataclass(frozen=True)
class GameMessageContent:
    _move_src: Location | None = field(default=None)
    _move_dest: Location | None = field(default=None)
    _player: Player | None = field(default=None)

    @classmethod
    def default_invalid(cls) -> GameMessageContent:
        return cls()

    @property
    def move_src(self) -> Location | None:
        return self._move_src

    @property
    def move_dest(self) -> Location | None:
        return self._move_dest

    @property
    def player(self) -> Player | None:
        return self._player


class GameMessage:
    _frame: int
    _message_type: GameMessageType
    _message_content: GameMessageContent

    def __init__(
        self,
        frame: int,
        message_type: GameMessageType,
        message_content: GameMessageContent,
    ) -> None:
        self._frame = frame
        self._message_type = message_type
        self._message_content = message_content

    @property
    def frame(self) -> int:
        return self._frame

    @property
    def message_type(self) -> GameMessageType:
        return self._message_type

    @property
    def message_content(self) -> GameMessageContent:
        return self._message_content


class GameMessageFactory:
    @classmethod
    def make(cls, data_raw: str) -> GameMessage:
        # ensure that data is a valid GameMessageDict; no way to actually enforce in Python other than this
        try:
            # todo: check if type hint actually holds
            data: GameMessageDict = json.loads(data_raw)
            """
            if type(data) != GameMessageDict:
                raise RuntimeError()
            """
        except json.JSONDecodeError:
            raise RuntimeError(
                'Error: JSON string does not follow the GameMessageDict format.'
            )
        except Exception:
            raise RuntimeError('Error: Unhandled exception in parsing JSON string.')

        frame: int = data['frame']
        message_type: GameMessageType = data['message_type']

        # make message content based on message type
        match message_type:
            case GameMessageType.MOVE:
                # todo: check if type hint actually holds
                message_content: MakeMoveGameMessageContentDict = data[
                    'message_content'
                ]
                """
                if type(message_content) != MakeMoveGameMessageContentDict:
                    raise RuntimeError(f'Error: GameMessage of type {message_type} has invalid message content.')
                """

                src: LocationDict = message_content['src']
                dest: LocationDict = message_content['dest']
                player: Player = message_content['player']

                return GameMessage(
                    frame,
                    message_type,
                    GameMessageContent(
                        _move_src=Location(src['row'], src['column']),
                        _move_dest=Location(dest['row'], dest['column']),
                        _player=player,
                    ),
                )
            case GameMessageType.INVALID:
                raise RuntimeError(
                    'Error: Value of message_type does not correspond to a valid message type.'
                )
