-module(tictactoe).
-compile(export_all).

start(PlayerOne, PlayerTwo) -> 
	spawn(tictactoe, loop, [PlayerOne, PlayerTwo, PlayerOne]).

make_move(GamePid, Player, Row, Col) ->
	GamePid ! { make_move, Player, Row, Col }.

loop(PlayerOne, PlayerTwo, CurrentPlayer) ->
	receive
		Message -> 
			io:format("PlayerOne: ~p~n PlayerTwo: ~p~n CurrentPlayer: ~p~n Message: ~p~n", [PlayerOne, PlayerTwo, CurrentPlayer, Message]),
			loop(PlayerOne, PlayerTwo, CurrentPlayer)
	end.