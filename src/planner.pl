:- module(planner, [detective_plan/1]).

:- use_module(map, [rooms/1]).

% Planner integration (fallback plan if planner not available)

detective_plan(Plan) :-
    (run_pyperplan(Plan) -> true ; default_plan(Plan)).

default_plan([
    move(detective,'Hall'),
    move(detective,'Kitchen'),
    inspect(player)
]).

run_pyperplan(Plan) :-
    catch(shell('python3 -m pyperplan adversary_domain.pddl adversary_problem.pddl > plan.txt'),_,fail),
    (exists_file('plan.txt') -> read_plan_file('plan.txt',Plan) ; fail).

read_plan_file(File, Plan) :-
    open(File,read,Stream),
    read_lines(Stream, Lines),
    close(Stream),
    maplist(parse_action, Lines, Plan).

read_lines(Stream, []) :- at_end_of_stream(Stream), !.
read_lines(Stream, [L|Ls]) :-
    read_line_to_codes(Stream, Codes), atom_codes(A,Codes), atom_string(A,S), normalize_space(string(L),S),
    read_lines(Stream, Ls).

parse_action(Line, move(detective,Room)) :-
    sub_atom(Line,_,_,_, 'move'),
    atomic_list_concat(['(', 'move', detective, RoomAtom, ')' ], ' ', Line),
    room_from_plan_atom(RoomAtom, Room).
parse_action(_, inspect(player)).

room_from_plan_atom(RoomAtom, Room) :-
    atom_string(RoomAtom, PlanString),
    normalize_token(PlanString, NormalizedPlan),
    rooms(Rooms),
    member(Room, Rooms),
    atom_string(Room, RoomString),
    normalize_token(RoomString, NormalizedPlan), !.

normalize_token(Str, Normalized) :-
    string_lower(Str, Lower),
    split_string(Lower, " _", " _", Parts),
    atomic_list_concat(Parts, '', Normalized).
