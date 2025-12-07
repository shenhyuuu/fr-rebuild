:- module(map, [
    rooms/1,
    rooms_grid/1,
    adjacent_room/3,
    path/2,
    shortest_distance/3,
    shortest_path_nodes/3,
    direction_delta/3,
    locate_room/4,
    within_grid/3
]).

:- use_module(library(lists)).

rooms([
    'Tower','Library','Armory','Observatory',
    'Hall','Dining Room','Kitchen','Storage',
    'Study','Throne Room','Bathroom','Bedroom',
    'Chapel','Dungeon','Wine Cellar','Balcony'
]).

rooms_grid([
    ['Tower','Library','Armory','Observatory'],
    ['Hall','Dining Room','Kitchen','Storage'],
    ['Study','Throne Room','Bathroom','Bedroom'],
    ['Chapel','Dungeon','Wine Cellar','Balcony']
]).

adjacent_room(Room, Direction, Adjacent) :-
    rooms_grid(Grid),
    locate_room(Grid, Room, Row, Col),
    direction_delta(Direction, DRow, DCol),
    Row1 is Row + DRow,
    Col1 is Col + DCol,
    within_grid(Grid, Row1, Col1),
    nth1(Row1, Grid, RowList),
    nth1(Col1, RowList, Adjacent).

path(Room, Adjacent) :-
    adjacent_room(Room, _, Adjacent).

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

shortest_path_nodes(Start, Goal, Path) :-
    bfs_path([(Start,[Start])], [], Goal, RevPath),
    reverse(RevPath, Path).

bfs_path([(Node,Path)|_], _, Goal, Path) :- Node == Goal, !.
bfs_path([(Node,Path)|Rest], Visited, Goal, ResultPath) :-
    findall((Next,[Next|Path]),
        ( path(Node,Next), \+ member(Next,Visited), \+ member(Next,Path)),
        Nexts),
    append(Rest, Nexts, Queue),
    append(Visited, [Node], NewVisited),
    bfs_path(Queue, NewVisited, Goal, ResultPath).

direction_delta(up, -1, 0).
direction_delta(down, 1, 0).
direction_delta(left, 0, -1).
direction_delta(right, 0, 1).

locate_room(Grid, Room, Row, Col) :-
    nth1(Row, Grid, RowList),
    nth1(Col, RowList, Room), !.

within_grid(Grid, Row, Col) :-
    Row > 0,
    Col > 0,
    length(Grid, RowCount),
    Row =< RowCount,
    nth1(1, Grid, FirstRow),
    length(FirstRow, ColCount),
    Col =< ColCount.
