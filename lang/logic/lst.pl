
lst_member(E, [E|_]).
lst_member(E, [_|Xs]) :- lst_member(E, Xs).

lst_prefix([], _).
lst_prefix([X|Xs], [X|Ys]) :- lst_prefix(Xs, Ys).

lst_suffix(Xs, Xs).
lst_suffix(Xs, [_|Ys]) :- lst_suffix(Xs, Ys).

lst_sublist(Xs, Ys) :- lst_prefix(Ps, Ys), lst_suffix(Xs, Ps).

lst_append([], Ys, Ys).
lst_append([X|Xs], Ys, [X|Zs]) :- lst_append(Xs, Ys, Zs).

lst_prefixa(Xs, Ys) :- lst_append(Xs, _, Ys).
lst_suffixa(Xs, Ys) :- lst_append(_, Xs, Ys).

lst_membera(X, Ys) :- lst_append(_, [X|_], Ys).
lst_member2(X, Ys) :- lst_sublist([X], Ys).

lst_adjacent(X, Y, Zs) :- lst_append(_, [X, Y | _], Zs).
lst_last(X, Zs) :- lst_append(_, [X], Zs).

lst_reverse(List, Tsil) :- lst_reverse(List, [], Tsil).
lst_reverse([], Tsil, Tsil).
lst_reverse([X|Xs], Acc, Tsil) :- lst_reverse(Xs, [X|Acc], Tsil).

twice([], []).
twice([X|Xs], [X,X|Ts]) :- twice(Xs, Ts).

sum([], 0).
sum([X|Xs], Sum) :- sum(Xs, Sum1), Sum is Sum1 + X.

lst_quicksort([], []).
lst_quicksort([X|Xs], Ys) :-
    lst_partition(Xs, X, Littles, Bigs),
    lst_quicksort(Littles, Ls),
    lst_quicksort(Bigs, Bs),
    lst_append(Ls, [X|Bs], Ys).

lst_partition([X|Xs], Y, [X|Ls], Bs) :- X =< Y, lst_partition(Xs, Y, Ls, Bs).
lst_partition([X|Xs], Y, Ls, [X|Bs]) :- X > Y, lst_partition(Xs, Y, Ls, Bs).
lst_partition([], _, [], []).

%% lst_subst(What, With, In, Out)
lst_subst(_, _, [], []).
lst_subst(X, Y, [X|Xs], [Y|Ys]) :- lst_subst(X, Y, Xs, Ys).
lst_subst(X, Y, [Z|Xs], [Z|Ys]) :- not(X = Z), lst_subst(X, Y, Xs, Ys).

%% lst_diff(FromList, WhatList, Result)
lst_diff([], _, []).
lst_diff([X|Xs], Ys, Zs) :- lst_member(X, Ys), lst_diff(Xs, Ys, Zs).
lst_diff([X|Xs], Ys, [X|Zs]) :- not(lst_member(X, Ys)), lst_diff(Xs, Ys, Zs).

%% split_at(List, Ind, Before, After)
split_at(L, 0, [], L).
split_at([X|Xs], I, [X|Bs], As) :-
    I > 0, I1 is I - 1,
    split_at(Xs, I1, Bs, As).

nth([X|_], 0, X).
nth([_|Xs], I, X) :- I > 0, I1 is I - 1, nth(Xs, I1, X).

lst_merge([], L, L).
lst_merge(L, [], L).
lst_merge([X|Xs], [Y|Ys], [X|Zs]) :- X =< Y, lst_merge(Xs, [Y|Ys], Zs).
lst_merge([X|Xs], [Y|Ys], [Y|Zs]) :- X > Y,  lst_merge([X|Xs], Ys, Zs).

mergesort([], []).
mergesort([X], [X]).
mergesort(List, Sorted) :-
    length(List, Len),
    split_at(List, Len//2, Bef, Aft),
    mergesort(Bef, SortedBef),
    mergesort(Aft, SortedAft),
    lst_merge(SortedBef, SortedAft, Sorted).

%% kth_largest(Xs, K)
% kth_largest(
