-module(gameclient).
-compile(export_all).

test() ->
	gameserver:start(),
	Player1 = login("player1"),
	Player2 = login("player2"),
	new_game(Player1, "player2"),
	make_move(Player1, "player2", "A", "1").

login(Name) ->
	Pid = spawn(gameclient, loop, [ Name ]),
	gameserver:connect(Name, Pid),
	Pid.

new_game(Pid, OpponentName) ->
	Pid ! {new_game, OpponentName}.

make_move(Pid, OpponentName, Row, Col) ->	
	Pid ! { make_move, OpponentName, Row, Col }.

loop(Name) ->
	receive
		{ msg, Message } ->
			loop(Name);
		{ new_game, OpponentName} ->
			gameserver:new_game(Name, OpponentName),
			loop(Name);
		{ make_move, OpponentName, Row, Col } ->
			gameserver:make_move(Name, OpponentName, Row, Col),
			loop(Name)			
	end.