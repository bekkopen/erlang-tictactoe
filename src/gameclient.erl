-module(gameclient).
-compile(export_all).

t() ->
	gameserver:start(),
	Player1 = login("player1"),
	Player2 = login("player2"),
	new_game(Player1, "player2"),

	make_move(Player1, "player2", a1),
	timer:sleep(5),
	make_move(Player1, "player2", b1),
	timer:sleep(5),
	make_move(Player2, "player1", b2),
	timer:sleep(5),
	make_move(Player1, "player2", a2),
	timer:sleep(5),	
	make_move(Player2, "player1", a2),
	timer:sleep(5),
	make_move(Player2, "player1", c3),
	timer:sleep(5),
	make_move(Player1, "player2", a3).


login(Name) ->
	Pid = spawn(gameclient, loop, [ Name ]),
	gameserver:connect(Name, Pid),
	Pid.

new_game(Pid, OpponentName) ->
	Pid ! {new_game, OpponentName}.

make_move(Pid, OpponentName, Move) ->	
	Pid ! { make_move, OpponentName, Move }.

game_tie(Pid, OpponentName, GameState) ->
	Pid ! { tie, OpponentName, GameState }.

send_message(Pid, Message) ->
	Pid ! { msg, Message}.

loop(Name) ->
	receive
		{ msg, Message } ->
			io:format("~p~n", [Message]),
			loop(Name);
		{ new_game, OpponentName} ->
			gameserver:new_game(Name, OpponentName),
			loop(Name);
		{ make_move, OpponentName, Move } ->
			gameserver:make_move(Name, OpponentName, Move),
			loop(Name);
		{ tie, OpponentName, GameState } ->
			io:format("~p~n", [get_tie_message(OpponentName)]),
			loop(Name)		
	end.

get_win_message(Over) ->
	"You won over " ++ Over ++ " :)".

get_loose_message(For) ->
	"You lost over " ++ For ++ "... :(".

get_tie_message(Opponent) ->
	"It is a tie between you and " ++ Opponent.	