:- module(map_view, [
    display_map/0
]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(map).
:- use_module(tasks, [task/6, task_status_label/2]).
:- use_module(world_state, [location/2]).

% Map rendering helpers

display_map :-
    nl,
    write('Map:'),nl,
    rooms_grid(Rows),
    maplist(maplist(cell_display), Rows, CellRows),
    findall(Len, (
        member(Row, CellRows),
        member(Cell, Row),
        member(Line, Cell),
        string_length(Line, Len)
    ), Lengths),
    max_list(Lengths, MaxCellLen0),
    MaxCellLen is max(12, MaxCellLen0),
    forall(member(RowCells, CellRows), (
        length(RowCells, Count),
        row_separator(Count, MaxCellLen, Sep),
        write(Sep), nl,
        render_row(RowCells, MaxCellLen, Lines),
        forall(member(Line, Lines), (write(Line), nl))
    )),
    CellRows = [FirstRow|_],
    length(FirstRow, FirstCount),
    row_separator(FirstCount, MaxCellLen, FinalSep),
    write(FinalSep), nl,
    nl.

row_separator(CellCount, CellWidth, Separator) :-
    ChunkWidth is CellWidth + 2,
    repeat_char(ChunkWidth, '-', Chunk),
    length(Chunks, CellCount),
    maplist(=(Chunk), Chunks),
    atomic_list_concat(Chunks, '+', Body),
    atomic_list_concat(['+', Body, '+'], Separator).

render_row(Cells, Width, Lines) :-
    Cells = [FirstCell|_],
    length(FirstCell, CellHeight),
    numlist(1, CellHeight, Indexes),
    maplist(row_line(Cells, Width), Indexes, Lines).

row_line(Cells, Width, Index, Line) :-
    maplist(nth1(Index), Cells, Texts),
    maplist(pad_cell(Width), Texts, Padded),
    atomic_list_concat(Padded, '|', Body),
    atomic_list_concat(['|', Body, '|'], Line).

pad_cell(Width, Text, Padded) :-
    string_length(Text, Len),
    Pad is Width - Len,
    repeat_char(Pad, ' ', Spaces),
    atomic_list_concat([' ', Text, Spaces, ' '], Padded).

repeat_char(N, Char, String) :-
    length(Chars, N),
    maplist(=(Char), Chars),
    atomics_to_string(Chars, '', String).

cell_display(Room, [RoomLine, TaskLine]) :-
    room_label(Room, Label),
    player_hint(Room, PH),
    task_hint(Room, TH),
    include(\=(""), [PH, TH], Hints),
    (   Hints = []
    ->  RoomLine = Label
    ;   atomics_to_string([Label|Hints], ' ', RoomLine)
    ),
    room_tasks_line(Room, TaskLine).

room_label(Room, Label) :- atom_string(Room, Label).

player_hint(Room, "(You are here)") :- location(player, Room), !.
player_hint(_, "").

task_hint(Room, "(Finished)") :- task(_,Room,_,_,complete,_), !.
task_hint(_, "").

room_tasks_line(Room, Line) :-
    findall(Display,
        ( task(TaskName,Room,_,_,Status,_),
          task_status_label(Status, StatusLabel),
          format(string(Display), "~w~w", [TaskName, StatusLabel])
        ),
        Tasks),
    (   Tasks = []
    ->  Line = ""
    ;   atomics_to_string(Tasks, ', ', TaskText),
        atomic_list_concat(['Task:', TaskText], ' ', Line)
    ).
