-module(tictactoe).
-compile(export_all).

start(PlayerOne, PlayerTwo) ->
	InitialGame = [a1, a2, a3, b1, b2, b3, c1, c2, c3], 
	spawn(tictactoe, loop, [PlayerOne, PlayerTwo, PlayerOne, InitialGame]).

make_move(GamePid, Player, Move) ->
	GamePid ! { make_move, Player, Move }.

loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState) ->
	{CurrentPid, CurrentName} = CurrentPlayer,
	{PlayerOnePid, PlayerOneName} = PlayerOne,
	{PlayerTwoPid, PlayerTwoName} = PlayerTwo,
	receive
		{ make_move, CurrentName, Move } ->			
			ValidMove = lists:any(fun(X) -> X =:= Move end, GameState),			
			if 
				ValidMove ->
					CurrentPlayerAtom = player_atom(PlayerOne, PlayerTwo, CurrentPlayer),
					UpdateFun = fun(Pos) ->
						value_if_match(Pos, Move, CurrentPlayerAtom)
					end,
					NewGameState = lists:map(UpdateFun, GameState),
					NewCurrentPlayer = change_player(PlayerOne, PlayerTwo, CurrentPlayer),
					io:format("State: ~p -> ~p~n", [GameState, NewGameState]),
						
					case get_game_result(NewGameState) of
						tie ->
							gameclient:game_tie(PlayerOnePid, PlayerTwoName, NewGameState),
							gameclient:game_tie(PlayerTwoPid, PlayerOneName, NewGameState);
						x ->
							gameclient:send_message(PlayerOnePid, get_win_message(PlayerTwoName)),
							gameclient:send_message(PlayerTwoPid, get_loose_message(PlayerOneName));
						o ->
							gameclient:send_message(PlayerTwoPid, get_win_message(PlayerOneName)),
							gameclient:send_message(PlayerOnePid, get_loose_message(PlayerTwoName));
						continue ->
							loop(PlayerOne, PlayerTwo, NewCurrentPlayer, NewGameState)
					end;								
				true ->
					gameclient:send_message(CurrentPid, "Position " ++ erlang:atom_to_list(Move) ++ " is not available."),
					loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState)
			end;

		{ make_move, WrongPlayerName, _ } ->
			WrongPlayerPid = get_pid_for_player_name(PlayerOne, PlayerTwo, WrongPlayerName),
			gameclient:send_message(WrongPlayerPid, "It is not your turn yet!"),
			loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState);
		Message -> 
			io:format("PlayerOne: ~p~n PlayerTwo: ~p~n CurrentPlayer: ~p~n Message: ~p~n", [PlayerOne, PlayerTwo, CurrentPlayer, Message]),
			loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState)
	end.

get_win_message(Over) ->
	"You won over " ++ Over ++ " :)".

get_loose_message(For) ->
	"You lost over " ++ For ++ "... :(".

get_game_result(GameState) ->
	case check_for_winner(GameState) of
		undecided ->
			GameOver = lists:all(fun(X) -> (X =:= x) or (X =:= o) end, GameState),
			if
				GameOver ->
					tie;
				true ->
					continue
			end;
		X -> X
	end.

check_for_winner([A1, A2, A3, B1, B2, B3, C1, C2, C3]) ->
	Rows = 		[[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
	Columns = 	[[A1, B1, C1], [A2, B2, C2], [A3, B3, C3]],
	Diagonals = [[A1, B2, C3], [A3, B2, C1]],
	get_winner(Rows ++ Columns ++ Diagonals).

get_winner([]) -> undecided;
get_winner([[X, X, X] | _]) -> X;
get_winner([_ | T]) -> get_winner(T).

value_if_match(X, X, NewValue) -> NewValue;
value_if_match(X, _, _) -> X.

player_atom(X, _, X) -> x;
player_atom(_, Y, Y) -> o.

change_player(PlayerOne, PlayerTwo, PlayerOne) -> PlayerTwo;
change_player(PlayerOne, PlayerTwo, PlayerTwo) -> PlayerOne.

get_pid_for_player_name({PlayerOnePid, WrongPlayerName}, _, WrongPlayerName) -> PlayerOnePid;
get_pid_for_player_name(_, {PlayerTwoPid, WrongPlayerName}, WrongPlayerName) -> PlayerTwoPid.

	