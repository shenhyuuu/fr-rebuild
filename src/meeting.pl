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
    discussion_phase,
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
        ; select_vote_by_trust(AI, Candidates, Vote))
    ; select_vote_by_trust(AI, Candidates, Vote)
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

select_vote_by_trust(_, [], _) :- !, fail.
select_vote_by_trust(AI, Candidates, Vote) :-
    findall(Trust-Target, (
        member(Target, Candidates),
        (trust(AI, Target, TVal) -> Trust = TVal ; Trust = 100)
    ), Pairs),
    keysort(Pairs, Sorted),
    Sorted = [Lowest-_|_],
    include(match_trust(Lowest), Sorted, LowestPairs),
    findall(Target, member(_-Target, LowestPairs), LowestTargets),
    random_member(Vote, LowestTargets).

match_trust(Val, Val-_).

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

discussion_phase :-
    write('--- Discussion phase ---'),nl,
    collect_statements,
    validate_statements.

collect_statements :-
    findall(Char, alive(Char), Participants),
    forall(member(Char, Participants), speak_from_log(Char)).

speak_from_log(Char) :-
    select_log_entry(Char, Round, Room, Others),
    assertz(log_spoken(Char, Round, Room)),
    assertz(history_log(Char, Round, Room, Others)),
    format_log_statement(Char, Round, Room, Others, Statement),
    format('~w~n', [Statement]).
speak_from_log(_).

select_log_entry(Char, Round, Room, Others) :-
    findall(log(R,Loc,Seen), (
        personal_log(Char, R, Loc, Seen),
        \+ log_spoken(Char, R, Loc),
        (Char == player -> \+ conflicts_with_history(R, Loc, Seen) ; true)
    ), Logs),
    Logs \= [],
    random_member(log(Round,Room,Others), Logs).

conflicts_with_history(Round, Room, Others) :-
    history_log(_, Round, Room, LoggedOthers),
    LoggedOthers \= Others.

format_log_statement(Char, Round, Room, Others, Statement) :-
    visible_name(Char, VisibleChar),
    display_names(Others, VisibleOthers),
    atomic_list_concat(VisibleOthers, ',', OthersText),
    format(atom(Statement), '[~w]在第[~w]轮在[~w]，该地方有[~w]。', [VisibleChar, Round, Room, OthersText]).

validate_statements :-
    findall(history_log(Speaker,R,Room,Others), history_log(Speaker,R,Room,Others), Statements),
    forall((alive(Observer), Observer \= player), validate_statements_for(Observer, Statements)).

validate_statements_for(_, []).
validate_statements_for(Observer, [history_log(Speaker,R,Room,Others)|Rest]) :-
    ( Observer \= Speaker,
      personal_log(Observer, R, Room, Seen)
    ->  (Seen == Others -> adjust_trust(Observer, Speaker, 5) ; adjust_trust(Observer, Speaker, -5))
    ;   true
    ),
    validate_statements_for(Observer, Rest).
