:- module(characters, [
    characters/1,
    role/2,
    alias/2,
    assign_aliases/0,
    visible_name/2,
    display_names/2,
    resolve_target/2
]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- dynamic alias/2.

characters([player,bunny1,bunny2,bunny3,bunny4,detective]).
role(player,fox).
role(bunny1,rabbit).
role(bunny2,rabbit).
role(bunny3,rabbit).
role(bunny4,rabbit).
role(detective,detective).

assign_aliases :-
    retractall(alias(_,_)),
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

visible_name(player, you) :- !.
visible_name(none, none) :- !.
visible_name(Char, Name) :-
    (alias(Char, Alias) -> Name = Alias ; Name = Char).

display_names(Chars, Names) :-
    maplist(visible_name, Chars, Names).

resolve_target(Input, Target) :-
    (alias(Target, Input) -> true ; Target = Input).
