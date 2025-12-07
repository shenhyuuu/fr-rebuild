:- module(meeting, [
    resolve_meeting/0,
    check_bodies/1
]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(characters, [display_names/2, visible_name/2, resolve_target/2]).
:- use_module(world_state).
:- use_module(tasks, [release_tasks_for/1]).
:- use_module(victory, [rabbits_win/0]).

resolve_meeting :-
    write('--- Meeting called ---'),nl,
    clear_bodies,
    run_votes,
    update_meeting_timer,
    clear_bodies,
    !.

check_bodies(Room) :-
    (   body(Room,_)
    ->  write('You spot a body here! A meeting will be triggered.'),nl,
        resolve_meeting
    ;   true
    ).

run_votes :-
    retractall(vote(_,_)),
    (alive(player) -> player_vote ; true),
    ai_votes,
    tally_votes.

player_vote :-
    write('Cast your vote (atom ending with period). alive characters: '),
    alive_rabbits(Rs), display_names(Rs, Visible), write(Visible),nl,
    read(V),
    (resolve_target(V, Target), alive(Target), Target \= player -> assertz(vote(player,Target)) ; write('Abstain.'),nl).

ai_votes :-
    forall((alive(AI), AI \= player), ai_single_vote(AI)).

ai_single_vote(AI) :-
    alive_targets_for_vote(AI, Candidates),
    (AI == detective ->
        (revealed_fox(Fox), alive(Fox) -> Vote = Fox
        ; random_vote(Candidates, Vote))
    ; random_vote(Candidates, Vote)
    ),
    assertz(vote(AI, Vote)),
    visible_name(AI, VisibleAI),
    visible_name(Vote, VisibleVote),
    format('~w votes for ~w.~n',[VisibleAI,VisibleVote]).

alive_targets_for_vote(AI, Candidates) :-
    findall(T, (alive(T), T \= AI), Candidates).

random_vote(Candidates, Vote) :-
    Candidates \= [],
    random_member(Vote, Candidates).

tally_votes :-
    findall(Target, vote(_,Target), Targets),
    count_targets(Targets,Counts),
    (Counts = [] -> write('No votes.'),nl ;
        keysort(Counts,Sorted), reverse(Sorted, [_-Target|_]),
        eliminate(Target)).

count_targets([],[]).
count_targets([H|T], Counts) :-
    count_targets(T,Partial),
    (select(N-H,Partial,Rest) -> N1 is N+1, Counts = [N1-H|Rest]
    ; Counts = [1-H|Partial]).

eliminate(Target) :-
    alive(Target),
    retract(alive(Target)),
    release_tasks_for(Target),
    visible_name(Target, VisibleTarget),
    (   Target == player
    ->  format('~w is ejected!~n',[VisibleTarget]),
        rabbits_win,
        halt
    ;   location(Target,_),
        format('~w is ejected!~n',[VisibleTarget])
    ).

update_meeting_timer :-
    retractall(next_meeting(_)),
    round_counter(R),
    NM is R + 3,
    retractall(next_meeting(_)),
    assertz(next_meeting(NM)),
    retractall(vote(_,_)),
    !.

clear_bodies :-
    retractall(body(_,_)).
