:- module(tasks, [
    task/6,
    task_status_label/2,
    task_specs/1,
    assign_tasks_to_rooms/0,
    progress_task/3,
    progress_task_if_owner/3,
    release_tasks_for/1,
    tasks_remaining/1,
    list_tasks_status/0,
    reset_tasks/0
]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(characters, [visible_name/2]).
:- use_module(map, [rooms/1]).
:- dynamic task/6.

% task(TaskId, Room, NeededRounds, RemainingRounds, Status, Occupant)

task_specs([
    spec(collect_food,4),
    spec(fix_wiring,5),
    spec(clean_vent,4),
    spec(fix_chandelier,3),
    spec('Organize Ancient Scrolls',2)
]).

assign_tasks_to_rooms :-
    reset_tasks,
    task_specs(Specs),
    rooms(Rooms),
    random_permutation(Rooms, Shuffled),
    length(Specs, Count),
    take(Count, Shuffled, SelectedRooms),
    assign_spec_to_room(Specs, SelectedRooms).

reset_tasks :-
    retractall(task(_,_,_,_,_,_)).

assign_spec_to_room([], []).
assign_spec_to_room([spec(Task,Need)|Specs], [Room|Rooms]) :-
    assertz(task(Task,Room,Need,Need,available,none)),
    assign_spec_to_room(Specs, Rooms).

progress_task(Task,Room,Actor) :-
    retract(task(Task,Room,Need,Remaining,in_progress,Actor)),
    NewR is Remaining - 1,
    (NewR =< 0 -> (
        assertz(task(Task,Room,Need,0,complete,none)),
        format('Task ~w completed!~n',[Task])
    ) ; assertz(task(Task,Room,Need,NewR,in_progress,Actor))).

progress_task_if_owner(AI,Task,Room) :-
    (task(Task,Room,_,_,in_progress,AI) -> progress_task(Task,Room,AI) ; true).

release_tasks_for(Actor) :-
    forall(task(T,R,N,Rem,in_progress,Actor), (
        retract(task(T,R,N,Rem,in_progress,Actor)),
        assertz(task(T,R,N,Rem,available,none))
    )).

tasks_remaining(Rem) :-
    findall(T, (task(T,_,_,R,Status,_), Status \= complete, R > 0), Ts),
    length(Ts, Rem).

list_tasks_status :-
    findall(desc(T,Room,Status,Remaining,Occupant), task(T,Room,_,Remaining,Status,Occupant), Descs),
    forall(member(desc(T,Room,S,R,O), Descs), (
        visible_name(O, VisibleO),
        format('Task ~w in ~w: ~w (~w rounds left, occupant ~w)~n', [T,Room,S,R,VisibleO])
    )).

% helpers

take(0, _, []).
take(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N-1,
    take(N1, T, Rest).

task_status_label(complete, " (done)") :- !.
task_status_label(_, "").
