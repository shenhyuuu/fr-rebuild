:- module(tufox, [
    start/0,
    move/1,
    look/0,
    status/0,
    kill/1,
    wait/0
]).

:- asserta(user:file_search_path(src, 'src')).
:- asserta(user:file_search_path(library, 'src')).

:- use_module(src(map)).
:- use_module(src(map_view)).
:- use_module(src(characters)).
:- use_module(src(tasks)).
:- use_module(src(world_state)).
:- use_module(src(meeting)).
:- use_module(src(ai_logic)).
:- use_module(src(victory)).
:- use_module(src(planner)).
:- use_module(src(game)).

start :- game:start.
move(Direction) :- game:move(Direction).
look :- game:look.
status :- game:status.
kill(Target) :- game:kill(Target).
wait :- game:wait.
