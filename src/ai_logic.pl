:- module(ai_logic, [
    ai_turns/0,
    ai_act/1,
    inspect_identity/1
]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(world_state).
:- use_module(map, [shortest_distance/3, shortest_path_nodes/3]).
:- use_module(tasks).
:- use_module(meeting, [resolve_meeting/0]).
:- use_module(planner, [detective_plan/1]).
:- use_module(characters, [visible_name/2, role/2]).

ai_turns :-
    alive_rabbits(Rs),
    forall(member(AI, Rs), ai_act(AI)),
    tick_world.

ai_act(AI) :- % dispatch for every AI agent
    ai_act_logic(AI).

ai_act_logic(AI) :-
    \+ alive(AI), !.

ai_act_logic(detective) :-
    location(detective,Room),
    ( body(Room,_) ->
        resolve_meeting
    ; ( cooldown(detective,inspect,CD),
        CD =:= 0,
        findall(T, (location(T,Room), alive(T), T \= detective, \+ inspected(T)), Targets),
        Targets \= [] ->
            Targets = [Target|_],
            inspect_identity(Target)
      ; execute_plan_step(detective)
      )
    ).

ai_act_logic(AI) :-
    location(AI,Room),
    (body(Room,_) ->
        resolve_meeting
    ; attempt_task(AI)).

inspect_identity(Target) :-
    role(Target, Role),
    assertz(inspected(Target)),
    retract(cooldown(detective,inspect,_)),
    assertz(cooldown(detective,inspect,2)),
    (Role == fox -> assertz(revealed_fox(Target)), resolve_meeting ; true),
    visible_name(Target, VisibleTarget),
    format('An inspection reveals ~w is ~w.~n',[VisibleTarget,Role]).

attempt_task(AI) :-
    (choose_task(AI, TargetTask, TargetRoom) ->
        location(AI,Room),
        (Room == TargetRoom ->
            (task(TargetTask,Room,_,_,available,none) ->
                retract(task(TargetTask,Room,N,R,available,none)),
                assertz(task(TargetTask,Room,N,R,in_progress,AI))
            ; progress_task_if_owner(AI,TargetTask,Room)
            )
        ; move_ai_toward(AI,TargetRoom)
        )
    ; true).

choose_task(AI, Task, Room) :-
    location(AI,Current),
    findall(dist(D,Task0,Room0),
        ( task(Task0,Room0,_,_,Status,Occupant),
          Status \= complete,
          preferred_task(AI, Status, Occupant),
          shortest_distance(Current, Room0, D)
        ),
        Distances),
    Distances \= [],
    closest_tasks(Distances, Closest),
    random_member(dist(_, Task, Room), Closest).

preferred_task(_, available, _).
preferred_task(AI, in_progress, Occupant) :- Occupant == AI.

closest_tasks(Distances, Closest) :-
    findall(D, member(dist(D,_,_), Distances), Ds),
    min_list(Ds, Min),
    include(matches_distance(Min), Distances, Closest).

matches_distance(Min, dist(D,_,_)) :- D =:= Min.

move_ai_toward(AI,TargetRoom) :-
    location(AI,Room),
    (   Room == TargetRoom
    ->  true
    ;   next_step(Room,TargetRoom,Next),
        retract(location(AI,Room)),
        assertz(location(AI,Next)),
        visible_name(AI, VisibleAI),
        format('~w moves to ~w.~n',[VisibleAI,Next])
    ;   true
    ).

next_step(Start,Goal,Next) :-
    shortest_path_nodes(Start, Goal, Path),
    Path = [Start,Next|_].

execute_plan_step(detective) :-
    detective_plan(Plan),
    (Plan = [Action|_] -> apply_action(detective, Action) ; true).

apply_action(_,move(detective,Room)) :-
    move_ai_toward(detective,Room).
apply_action(_,inspect(Target)) :-
    inspect_identity(Target).
