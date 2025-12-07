:- module(world_state, [
    location/2,
    alive/1,
    cooldown/3,
    inspected/1,
    body/2,
    next_meeting/1,
    round_counter/1,
    revealed_fox/1,
    vote/2,
    trust/3,
    personal_log/4,
    history_log/4,
    log_spoken/3,
    reset_world/0,
    assign_initial_locations/0,
    alive_rabbits/1,
    tick_world/0,
    show_cooldowns/0,
    decrement_cooldowns/0,
    adjust_trust/3
]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(map, [rooms/1]).
:- use_module(characters, [characters/1, assign_aliases/0, role/2]).
:- use_module(tasks).

:- dynamic location/2.
:- dynamic alive/1.
:- dynamic cooldown/3.
:- dynamic inspected/1.
:- dynamic body/2.
:- dynamic next_meeting/1.
:- dynamic round_counter/1.
:- dynamic revealed_fox/1.
:- dynamic vote/2.
:- dynamic trust/3.
:- dynamic personal_log/4.
:- dynamic history_log/4.
:- dynamic log_spoken/3.

reset_world :-
    retractall(location(_,_)),
    retractall(alive(_)),
    retractall(cooldown(_,_,_)),
    retractall(inspected(_)),
    retractall(body(_,_)),
    retractall(next_meeting(_)),
    retractall(round_counter(_)),
    retractall(revealed_fox(_)),
    retractall(vote(_,_)),
    retractall(trust(_,_,_)),
    retractall(personal_log(_,_,_,_)),
    retractall(history_log(_,_,_,_)),
    retractall(log_spoken(_,_,_)),
    assign_tasks_to_rooms,
    characters(Chars),
    forall(member(C,Chars), assertz(alive(C))),
    assign_aliases,
    assign_initial_locations,
    init_trust_values,
    assertz(cooldown(player,kill,0)),
    assertz(cooldown(detective,inspect,2)),
    assertz(next_meeting(3)),
    assertz(round_counter(0)).

assign_initial_locations :-
    rooms(Rooms),
    random_member(PlayerRoom, Rooms),
    assertz(location(player, PlayerRoom)),
    repeat,
        random_member(DetectiveRoom, Rooms),
        DetectiveRoom \= PlayerRoom,
    !,
    assertz(location(detective, DetectiveRoom)),
    findall(Bunny, role(Bunny, rabbit), Bunnies),
    assign_bunny_locations(Rooms, Bunnies).

assign_bunny_locations(_, []) :- !.
assign_bunny_locations(Rooms, [Bunny|Rest]) :-
    random_member(Room, Rooms),
    assertz(location(Bunny, Room)),
    assign_bunny_locations(Rooms, Rest).

alive_rabbits(List) :-
    findall(R, (alive(R), R \= player), List).

tick_world :-
    decrement_cooldowns,
    round_counter(R),
    R1 is R+1,
    retract(round_counter(_)),
    assertz(round_counter(R1)),
    record_round_logs(R1),
    format('--- Round ~w ---~n', [R1]).

show_cooldowns :-
    forall(member(Skill, [kill]), (
        cooldown(player, Skill, V), format('Cooldown ~w: ~w~n',[Skill,V])
    )).

decrement_cooldowns :-
    forall(cooldown(Char,Skill,CD), (
        New is max(0, CD-1),
        retract(cooldown(Char,Skill,CD)),
        assertz(cooldown(Char,Skill,New))
    )),
    forall(task(T,R,N,Rem,in_progress,Occ), (
        (   alive(Occ)
        ->  progress_task(T,R,Occ)
        ;   retract(task(T,R,N,Rem,in_progress,Occ)),
            assertz(task(T,R,N,Rem,available,none))
        )
    )).

init_trust_values :-
    characters(Chars),
    forall(member(A, Chars), (
        forall((member(B, Chars), A \= B), assertz(trust(A,B,100)))
    )).

record_round_logs(Round) :-
    findall(Char, alive(Char), AliveChars),
    forall(member(Char, AliveChars), record_single_log(Char, Round)).

record_single_log(Char, Round) :-
    location(Char, Room),
    findall(Other, (location(Other, Room), alive(Other), Other \= Char), Others0),
    sort(Others0, Others),
    reconcile_logs(Round, Room, Char, Others).

reconcile_logs(Round, Room, Char, Others) :-
    findall(O, personal_log(O, Round, Room, _), ExistingOwners),
    list_to_set([Char|ExistingOwners], Owners),
    retractall(personal_log(_, Round, Room, _)),
    forall(member(O, Owners), assertz(personal_log(O, Round, Room, Others))).

adjust_trust(Observer, Target, Delta) :-
    ( trust(Observer, Target, Value)
    ->  NewValue is max(0, min(200, Value + Delta)),
        retract(trust(Observer, Target, Value)),
        assertz(trust(Observer, Target, NewValue))
    ;   true
    ).
