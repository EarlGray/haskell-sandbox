natural_number(o).
natural_number(s(X)) :- natural_number(X).

num_int(o, 0).
num_int(s(X), SI) :- num_int(X, I), SI is I + 1.

leq(o, X) :- natural_number(X).
leq(s(X), s(Y)) :- leq(X, Y).

less(o, s(X)) :- natural_number(X).
less(s(X), s(Y)) :- less(X, Y).

plus(o, X, X) :- natural_number(X).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).

times(o, X, o) :- natural_number(X).
times(s(X), Y, Z) :- times(X, Y, XY), plus(XY, Y, Z).

exp(s(X), o, o) :- natural_number(X).
exp(o, s(X), s(o)) :- natural_number(X).
exp(s(N), X, Y) :- exp(N, X, Z), times(Z, X, Y).

% X = Y * _ + Z
nat_mod(X, Y, Z) :- less(Z, Y), times(Y, _, QY), plus(QY, Z, X).

add(X, Y, Z) :- num_int(XN, X), num_int(YN, Y), num_int(ZN, Z), plus(XN, YN, ZN).

factorial(o, s(o)).
factorial(s(N), F) :- factorial(N, F1), times(s(N), F1, F).

nat_min(N1, N2, N1) :- leq(N1, N2).
nat_min(N1, N2, N2) :- leq(N2, N1).

even(o).
even(s(s(X))) :- even(X).
odd(s(o)).
odd(s(s(X))) :- odd(X).


%%% binary numbers

binary_number(o).
binary_number(zero(X)) :- binary_number(X).
binary_number(one(X))  :- binary_number(X).

bnum_int(o, 0).
bnum_int(zero(X), I) :- bnum_int(X, I1), I1 is I + I.
bnum_int(one(X), I) :- bnum_int(X, I1), I1 is I + I + 1.
