:- module(victory, [
    fox_win/0,
    rabbits_win/0
]).

fox_win :-
    write('You have reduced the rabbits. Fox wins!'),nl.

rabbits_win :-
    write('Rabbits completed objectives. Rabbits win!'),nl.
