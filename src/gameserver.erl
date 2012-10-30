-module(gameserver).
-compile(export_all).
-define(SERVER, gameserver).

start() ->
    server_util:start(?SERVER, 
    				  { gameserver, game_loop, 
    	  			    [ dict:new(), dict:new() ]}).

connect(Name, Pid) ->
	global:send(?SERVER, { connect, Name, Pid }).

new_game(Name, OpponentName) ->
	global:send(?SERVER, { new_game, Name, OpponentName }).

make_move(Name, OpponentName, Move) ->
	global:send(?SERVER, { make_move, Name, OpponentName, Move }).

game_loop(Players, Games) ->
    process_flag(trap_exit, true),
	receive
		{ connect, Name, Pid } ->
			Pid ! { msg, "Welcome to tictactoe server!" },
			game_loop(dict:store(Name, Pid, Players), Games);

		{ new_game, Name, OpponentName } ->
			case dict:find(Name, Players) of
                { ok, Pid } ->
                    case dict:find(OpponentName, Players) of
                    	{ ok, OpponentPid} ->
                    		Pid ! { msg, "Ready to rumble against " ++ OpponentName ++ "!" },
                    		OpponentPid ! { msg, "Ready to rumble against " ++ Name ++ "!" },
                    		GamePid = tictactoe:start({Pid, Name}, {OpponentPid, OpponentName}),
                    		GameKey = create_game_key(Name, OpponentName),
                            link(GamePid),
                    		game_loop(Players, dict:store(GameKey, GamePid, Games));
                    	error ->
                    		Pid ! { msg, "Did not find opponent " ++ OpponentName ++ "!" },
                    		game_loop(Players, Games)
                    end;
                error ->
                    io:format("Could not find player ~p~n", [ Name ]),
                    game_loop(Players, Games)
            end;

        { make_move, Name, OpponentName, Move } ->
			case dict:find(create_game_key(Name, OpponentName), Games) of
                { ok, GamePid } ->
                	tictactoe:make_move(GamePid, Name, Move),
                	game_loop(Players, Games);
                error ->
                	game_loop(Players, Games)
            end;
        {'EXIT', GamePid, _} ->
            io:format("Removing game ~p~n", [GamePid]),
            game_loop(Players, remove_game(GamePid, Games));
	    Oops ->
 			io:format("I don't get ~p~n", [ Oops ]),
 			game_loop(Players, Games)
 	end.

remove_game(GamePid, Games) ->
    dict:filter(fun(_, Value) -> Value =/= GamePid end, Games).

create_game_key(PlayerOne, PlayerTwo) ->
	string:join(lists:sort([PlayerOne, PlayerTwo]), "").