-module(tictactoe).
-compile(export_all).

start(PlayerOne, PlayerTwo) ->
	InitialGame = [a1, a2, a3, b1, b2, b3, c1, c2, c3], 
	spawn(tictactoe, loop, [PlayerOne, PlayerTwo, PlayerOne, InitialGame]).

make_move(GamePid, Player, Move) ->
	GamePid ! { make_move, Player, Move }.

loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState) ->
	receive
		Message -> 
			io:format("PlayerOne: ~p~n PlayerTwo: ~p~n CurrentPlayer: ~p~n Message: ~p~n", [PlayerOne, PlayerTwo, CurrentPlayer, Message]),
			loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState)
	end.