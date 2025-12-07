:- module(game, [
    start/0,
    move/1,
    look/0,
    status/0,
    kill/1,
    wait/0
]).

:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(map).
:- use_module(map_view, [display_map/0]).
:- use_module(characters).
:- use_module(tasks).
:- use_module(world_state).
:- use_module(meeting, [resolve_meeting/0, check_bodies/1]).
:- use_module(ai_logic, [ai_turns/0]).
:- use_module(victory).

start :-
    reset_world,
    format('\nWelcome to TuFox!\n'),
    write('You are the fox. Eliminate rabbits until only one remains.'),nl,
    write('Rabbits win if tasks finish or the fox dies.'),nl,
    print_help,
    look,
    game_loop.

print_help :-
    nl,
    write('Commands:'),nl,
    write('  move(Direction).         % move up/down/left/right on the map'),nl,
    write('  look.                    % describe current room'),nl,
    write('  status.                  % show game status'),nl,
    write('  kill(Target).            % eliminate a rabbit in this room (cooldown 3)'),nl,
    write('  wait.                    % end your action'),nl,
    nl.

look :-
    location(player,Room),
    format('You are in the ~w.~n', [Room]),
    print_connected(Room),
    print_room_state(Room),
    display_map.

print_connected(Room) :-
    findall(Dest, path(Room, Dest), Ds),
    list_to_set(Ds, Unique),
    format('Connected rooms: ~w~n', [Unique]).

print_room_state(Room) :-
    findall(C, (location(C,Room), alive(C), C \= player), Others),
    display_names(Others, VisibleOthers),
    (VisibleOthers = [] -> write('No other characters here.\n')
    ; format('Others here: ~w~n', [VisibleOthers])),
    (body(Room,V) -> (visible_name(V,VisibleV), format('There is a body here: ~w~n',[VisibleV])) ; true),
    findall(T, (task(T,Room,_,Remaining,Status,_), member(Status,[available,in_progress]), Remaining>0), Tasks),
    (Tasks = [] -> true ; format('Active tasks here: ~w~n',[Tasks]) ).

status :-
    round_counter(R),
    format('Round ~w.~n', [R]),
    alive_rabbits(AliveRabbits),
    display_names(AliveRabbits, VisibleRabbits),
    format('Alive rabbits: ~w~n', [VisibleRabbits]),
    (alive(player) -> write('You are alive.\n'); write('You are dead.\n')),
    list_tasks_status,
    display_map,
    show_cooldowns.

move(Direction) :-
    alive(player),
    location(player,From),
    (   adjacent_room(From, Direction, To)
    ->  retract(location(player,From)),
        assertz(location(player,To)),
        format('You moved to ~w.~n',[To]),
        check_bodies(To),
        player_done
    ;   valid_direction(Direction)
    ->  write('Cannot move in that direction from here.'),nl, player_turn
    ;   write('Unknown direction. Use up/down/left/right.'),nl, player_turn
    ).

valid_direction(up).
valid_direction(down).
valid_direction(left).
valid_direction(right).

wait :-
    alive(player),
    write('You wait.'),nl,
    player_done.

kill(Target) :-
    alive(player),
    cooldown(player,kill,CD),
    (CD > 0 -> format('Kill skill cooling down (~w).~n',[CD]), player_turn
    ; resolve_target(Target, Resolved),
      location(player,Room), location(Resolved,Room), alive(Resolved), Resolved \= player ->
        retract(alive(Resolved)),
        release_tasks_for(Resolved),
        assertz(body(Room,Resolved)),
        retract(cooldown(player,kill,_)),
        assertz(cooldown(player,kill,3)),
        visible_name(Resolved, Visible),
        format('You eliminated ~w!~n',[Visible]),
        player_done
    ; write('No valid target here.'),nl, player_turn).

player_done :-
    ai_turns,
    game_loop.

player_turn :-
    (alive(player) -> true ; write('You are dead. Watching the chaos...'),nl, player_done),
    read_command(Command),
    (Command == quit -> halt ; (catch(call(Command), Err, (print_message(error, Err), player_turn)))).

read_command(Command) :-
    prompt('|: ', ''),
    read_line_to_string(user_input, Raw),
    normalize_space(string(Trimmed), Raw),
    (Trimmed == "" -> read_command(Command)
    ; ensure_period(Trimmed, WithPeriod),
      (   catch(read_term_from_atom(WithPeriod, Command0, [variable_names(Vars)]), error(syntax_error(_),_), fail)
      ->  bind_variable_names(Vars),
          Command = Command0
      ;   write('Could not parse that command. Try syntax like look. or kill(bunny1).'),nl,
          read_command(Command)
      )
    ).

bind_variable_names([]).
bind_variable_names([Name=Var|Rest]) :-
    ( var(Var) -> Var = Name ; true ),
    bind_variable_names(Rest).

ensure_period(Str, Str) :-
    sub_string(Str, _, 1, 0, '.'), !.
ensure_period(Str, WithPeriod) :-
    string_concat(Str, '.', WithPeriod).

player_turn_entry :-
    player_turn.

game_loop :-
    (check_victory -> true ;
        round_counter(R),
        next_meeting(NM),
        (R >= NM -> resolve_meeting ; true),
        (alive(player) -> player_turn ; (ai_turns, game_loop))
    ).

check_victory :-
    (\+ alive(player) -> rabbits_win, true
    ; alive_rabbits(List), length(List,L), (L =< 1 -> fox_win, true ;
        tasks_remaining(Rem), (Rem =< 0 -> rabbits_win, true ; fail))).
