% TuFox text adventure game with AI rabbits and planner-driven detective

:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(library(apply)).

:- dynamic location/2.
:- dynamic alive/1.
:- dynamic role/2.
:- dynamic task/6.
:- dynamic cooldown/3.
:- dynamic inspected/1.
:- dynamic body/2.
:- dynamic next_meeting/1.
:- dynamic round_counter/1.
:- dynamic revealed_fox/1.
:- dynamic vote/2.
:- dynamic alias/2.

room_layout([
    [tower, library, armory, observatory],
    [hall, dining_room, kitchen, storage],
    [study, throne_room, bathroom, bedroom],
    [chapel, dungeon, wine_cellar, balcony]
]).

rooms(Rooms) :-
    room_layout(Layout),
    append(Layout, Rooms).

room_coords(Room, Row, Col) :-
    room_layout(Layout),
    nth1(Row, Layout, RowList),
    nth1(Col, RowList, Room).

path(A,B) :-
    room_coords(A,RowA,ColA),
    room_coords(B,RowB,ColB),
    Diff is abs(RowA-RowB) + abs(ColA-ColB),
    Diff =:= 1.

% task(TaskId, Room, NeededRounds, RemainingRounds, Status, Occupant)
task_definitions([
    task_def(collect_food,4),
    task_def(fix_wiring,5),
    task_def(clean_vent,4),
    task_def(fix_chandelier,3),
    task_def(organize_ancient_scrolls,2)
]).

room_label(tower, 'Tower').
room_label(library, 'Library').
room_label(armory, 'Armory').
room_label(observatory, 'Observatory').
room_label(hall, 'Hall').
room_label(dining_room, 'Dining Room').
room_label(kitchen, 'Kitchen').
room_label(storage, 'Storage').
room_label(study, 'Study').
room_label(throne_room, 'Throne Room').
room_label(bathroom, 'Bathroom').
room_label(bedroom, 'Bedroom').
room_label(chapel, 'Chapel').
room_label(dungeon, 'Dungeon').
room_label(wine_cellar, 'Wine Cellar').
room_label(balcony, 'Balcony').

task_label(collect_food, 'Collect Food').
task_label(fix_wiring, 'Fix Wiring').
task_label(clean_vent, 'Clean Vent').
task_label(fix_chandelier, 'Fix Chandelier').
task_label(organize_ancient_scrolls, 'Organize Ancient Scrolls').

assign_tasks_to_rooms :-
    task_definitions(Defs),
    rooms(Rooms),
    random_permutation(Rooms, Shuffled),
    length(Defs, Count),
    length(Picked, Count),
    append(Picked, _, Shuffled),
    assign_task_rooms(Defs, Picked).

assign_task_rooms([], []).
assign_task_rooms([task_def(Id,Need)|Defs], [Room|Rooms]) :-
    assertz(task(Id,Room,Need,Need,available,none)),
    assign_task_rooms(Defs, Rooms).

characters([player,bunny1,bunny2,bunny3,bunny4,detective]).
role(player,fox).
role(bunny1,rabbit).
role(bunny2,rabbit).
role(bunny3,rabbit).
role(bunny4,rabbit).
role(detective,detective).

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
    write('  move(Room).              % move to connected room'),nl,
    write('  look.                    % describe current room'),nl,
    write('  status.                  % show game status'),nl,
    write('  perform(Task).           % work on a task in this room'),nl,
    write('  kill(Target).            % eliminate a rabbit in this room (cooldown 3)'),nl,
    write('  inspect(Target).         % if detective is AI, player cannot inspect'),nl,
    write('  call_meeting.            % call emergency meeting'),nl,
    write('  vote(Target).            % vote during meetings'),nl,
    write('  wait.                    % end your action'),nl,
    nl.

reset_world :-
    retractall(location(_,_)),
    retractall(alive(_)),
    retractall(task(_,_,_,_,_,_)),
    retractall(cooldown(_,_,_)),
    retractall(inspected(_)),
    retractall(body(_,_)),
    retractall(next_meeting(_)),
    retractall(round_counter(_)),
    retractall(revealed_fox(_)),
    retractall(vote(_,_)),
    retractall(alias(_,_)),
    assign_tasks_to_rooms,
    forall(characters(Cs), (forall(member(C,Cs), assertz(alive(C))))),
    assign_aliases,
    assign_initial_locations,
    assertz(cooldown(player,kill,0)),
    assertz(cooldown(detective,inspect,2)),
    assertz(next_meeting(3)),
    assertz(round_counter(0)).

assign_aliases :-
    characters(Chars),
    exclude(=(player), Chars, NonPlayer),
    length(NonPlayer, Count),
    numlist(1, Count, Numbers),
    maplist(number_alias, Numbers, Aliases),
    random_permutation(Aliases, Shuffled),
    pair_aliases(NonPlayer, Shuffled).

number_alias(N, AliasAtom) :-
    atomic_list_concat([rabbit, N], AliasAtom).

pair_aliases([], []).
pair_aliases([C|Cs], [A|As]) :-
    assertz(alias(C,A)),
    pair_aliases(Cs, As).

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
    ; format('Others here: ~w\n', [VisibleOthers])),
    (body(Room,V) -> (visible_name(V,VisibleV), format('There is a body here: ~w\n',[VisibleV])) ; true),
    findall(Label, (task(T,Room,_,Remaining,Status,_), member(Status,[available,in_progress]), Remaining>0, task_label(T,Label)), Labels),
    (Labels = [] -> true ; format('Active tasks here: ~w\n',[Labels]) ).

display_map :-
    nl,
    write('Map (● you, ✗ unstarted task, ✓ completed task):'),nl,
    room_layout(Layout),
    location(player, PlayerRoom),
    forall(member(Row, Layout), (
        maplist(room_cell(PlayerRoom), Row, Cells),
        atomic_list_concat(Cells, ' ', Line),
        write(Line), nl
    )),
    nl.

room_cell(PlayerRoom, Room, Cell) :-
    room_symbol(Room, PlayerRoom, Symbol),
    room_label(Room, Label),
    (Symbol == '' -> format(string(Cell), '[~w]', [Label])
    ; format(string(Cell), '[~w ~w]', [Label,Symbol])
    ).

room_symbol(Room, Room, '●') :- !.
room_symbol(Room, _, '✓') :-
    task(_,Room,_,_,complete,_), !.
room_symbol(Room, _, '✗') :-
    task(_,Room,_,_,available,_), !.
room_symbol(_, _, '').

status :-
    round_counter(R),
    format('Round ~w.~n', [R]),
    alive_rabbits(AliveRabbits),
    display_names(AliveRabbits, VisibleRabbits),
    format('Alive rabbits: ~w~n', [VisibleRabbits]),
    (alive(player) -> write('You are alive.\n'); write('You are dead.\n')),
    display_map,
    list_tasks_status,
    show_cooldowns.

list_tasks_status :-
    findall(desc(T,Room,Status,Remaining,Occupant), task(T,Room,_,Remaining,Status,Occupant), Descs),
    forall(member(desc(T,Room,S,R,O), Descs), (
        visible_name(O, VisibleO),
        task_label(T, Label),
        room_label(Room, RoomLabel),
        format('Task ~w in ~w: ~w (~w rounds left, occupant ~w)~n', [Label,RoomLabel,S,R,VisibleO])
    )).

show_cooldowns :-
    forall(member(Skill, [kill]), (
        cooldown(player, Skill, V), format('Cooldown ~w: ~w~n',[Skill,V])
    )).

move(To) :-
    alive(player),
    location(player,From),
    (path(From,To) -> (
        retract(location(player,From)),
        assertz(location(player,To)),
        format('You moved to ~w.~n',[To]),
        check_bodies(To),
        player_done
    ) ; (write('Cannot move there from here.'),nl, player_turn)).

wait :-
    alive(player),
    write('You wait.'),nl,
    player_done.

perform(Task) :-
    alive(player),
    location(player,Room),
    (task(Task,Room,Need,Remaining,Status,Occupant) -> (
        (Status == available ->
            retract(task(Task,Room,Need,Remaining,Status,Occupant)),
            NewR is Need - 1,
            assertz(task(Task,Room,Need,NewR,in_progress,player)),
            write('You start the task.'),nl,
            player_done
        ; Status == in_progress, Occupant == player ->
            progress_task(Task,Room,player),
            player_done
        ; Status == in_progress ->
            write('Someone else is on that task.'),nl, player_turn
        ; Status == complete ->
            write('Task already complete.'),nl, player_turn)
    ) ; (write('No such task here.'),nl, player_turn)).

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

inspect(_) :-
    write('Only the AI detective inspects identities.'),nl,
    player_turn.

call_meeting :-
    write('You report a body and call a meeting.'),nl,
    resolve_meeting,
    player_done.

vote(_) :-
    write('Voting only happens during meetings.'),nl,
    player_turn.

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
      (   catch(read_term_from_atom(WithPeriod, Command, []), error(syntax_error(_),_), fail)
      ->  true
      ;   write('Could not parse that command. Try syntax like look. or kill(bunny1).'),nl,
          read_command(Command)
      )
    ).

ensure_period(Str, Str) :-
    sub_string(Str, _, 1, 0, '.'), !.
ensure_period(Str, WithPeriod) :-
    string_concat(Str, '.', WithPeriod).

visible_name(player, you) :- !.
visible_name(none, none) :- !.
visible_name(Char, Name) :-
    (alias(Char, Alias) -> Name = Alias ; Name = Char).

display_names(Chars, Names) :-
    maplist(visible_name, Chars, Names).

resolve_target(Input, Target) :-
    (alias(Target, Input) -> true ; Target = Input).

progress_task(Task,Room,Actor) :-
    retract(task(Task,Room,Need,Remaining,in_progress,Actor)),
    NewR is Remaining - 1,
    (NewR =< 0 -> (
        assertz(task(Task,Room,Need,0,complete,none)),
        task_label(Task, Label),
        format('~w 已完成!~n',[Label])
    ) ; assertz(task(Task,Room,Need,NewR,in_progress,Actor))).

check_bodies(Room) :-
    (body(Room,_) -> write('You spot a body here! You may call a meeting.'),nl ; true).

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

alive_rabbits(List) :-
    findall(R, (alive(R), R \= player), List).

fox_win :-
    write('You have reduced the rabbits. Fox wins!'),nl.

rabbits_win :-
    write('Rabbits completed objectives. Rabbits win!'),nl.

tasks_remaining(Rem) :-
    findall(T, (task(T,_,_,R,Status,_), Status \= complete, R > 0), Ts),
    length(Ts, Rem).

% AI logic
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

progress_task_if_owner(AI,Task,Room) :-
    (task(Task,Room,_,_,in_progress,AI) -> progress_task(Task,Room,AI) ; true).

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

shortest_distance(Room, Room, 0) :- !.
shortest_distance(Start, Goal, Dist) :-
    bfs_queue([(Start,0)], [Start], Goal, Dist).

bfs_queue([(Node,D)|_], _, Goal, D) :- Node == Goal, !.
bfs_queue([(Node,D)|Rest], Visited, Goal, Dist) :-
    findall((Next,D1), (path(Node,Next), \+ member(Next,Visited), D1 is D+1), Nexts),
    findall(Next, member((Next,_), Nexts), NextRooms),
    append(Rest, Nexts, Queue),
    append(Visited, NextRooms, NewVisited),
    bfs_queue(Queue, NewVisited, Goal, Dist).

move_ai_toward(AI,TargetRoom) :-
    location(AI,Room),
    (path(Room,TargetRoom) -> Next = TargetRoom ; find_path(Room,TargetRoom,[Room],Next)),
    (nonvar(Next) -> (
        retract(location(AI,Room)),
        assertz(location(AI,Next)),
        visible_name(AI, VisibleAI),
        format('~w moves to ~w.~n',[VisibleAI,Next])
    ) ; true).

find_path(Start,Goal,Visited,Next) :-
    path(Start,Mid), \+ member(Mid,Visited),
    (Mid == Goal -> Next = Mid ; find_path(Mid,Goal,[Mid|Visited],Next)).

resolve_meeting :-
    write('--- Meeting called ---'),nl,
    clear_bodies,
    run_votes,
    update_meeting_timer,
    clear_bodies,
    !.

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

% world tick: cooldown reductions and task progress persistence

tick_world :-
    decrement_cooldowns,
    round_counter(R),
    R1 is R+1,
    retract(round_counter(_)),
    assertz(round_counter(R1)),
    print_round(R1).

print_round(R) :-
    format('--- Round ~w ---~n', [R]).

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

release_tasks_for(Actor) :-
    forall(task(T,R,N,Rem,in_progress,Actor), (
        retract(task(T,R,N,Rem,in_progress,Actor)),
        assertz(task(T,R,N,Rem,available,none))
    )).

% Planner integration (fallback plan if planner not available)

execute_plan_step(detective) :-
    plan_for_detective(Plan),
    (Plan = [Action|_] -> apply_action(detective, Action) ; true).

plan_for_detective(Plan) :-
    (run_pyperplan(Plan) -> true ; default_plan(Plan)).

default_plan([
    move(detective,hall),
    move(detective,kitchen),
    inspect(player)
]).

apply_action(_,move(detective,Room)) :-
    move_ai_toward(detective,Room).
apply_action(_,inspect(Target)) :-
    inspect_identity(Target).

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
    atom_string(Room, RoomAtom).
parse_action(_, inspect(player)).
