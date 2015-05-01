%% polynomial(Expr, X)

polynomial(X, X).
polynomial(Term, _) :- constant(Term). 
polynomial(Term1+Term2, X) :-
    polynomial(Term1, X), polynomial(Term2, X).
polynomial(Term1-Term2, X) :-
    polynomial(Term1, X), polynomial(Term2, X).
polynomial(Term1*Term2, X) :-
    polynomial(Term1, X), polynomial(Term2, X).
polynomial(Term1/Term2, X) :-
    polynomial(Term1, X), polynomial(Term2, X).
polynomial(Term ^ N, X) :-    
    natural_number(N), polynomial(Term, X).

derivative(X, X, 1).
derivative(X ^ N, X, N * (X ^ N1)) :- N1 is N - 1.
derivative(sin(X), X, cos(X)).
derivative(cos(X), X, -sin(X)).
derivative(e ^ X, X, e ^ X).
derivative(log(X), X, 1/X).

derivative(F + G, X, DF + DG) :-
    derivative(F, X, DF), derivative(G, X, DG).
derivative(F - G, X, DF - DG) :-
    derivative(F, X, DF), derivative(G, X, DG).
derivative(F * G, X, F * DG + DF * G) :-
    derivative(F, X, DF), derivative(G, X, DG).
derivative(1/F, X, -DF/(F * F)) :-
    derivative(F, X, DF).
derivative(F / G, X, (G*DF - F*DG)/(G*G)) :-
    derivative(F, X, DF), derivative(G, X, DG).

%% simplify

simplify(X + 0, SX) :- simplify(X, SX).
simplify(0 + X, SX) :- simplify(X, SX).

simplify(X * 1, SX) :- simplify(X, SX).
simplify(1 * X, SX) :- simplify(X, SX).
simplify(X / 1, SX) :- simplify(X, SX).

simplify(_ * 0, 0).
simplify(0 * _, 0).
simplify(0 / X, 0) :- not(X = 0).

simplify(-X * -Y, SX * SY) :- simplify(X, SX), simplify(Y, SY).
simplify(X * -Y, - SX * SY) :- simplify(X, SX), simplify(Y, SY).

simplify(X ^ 1, SX) :- simplify(X, SX).
simplify(_ ^ 0, 1).

simplify(X * X, SX ^ 2) :- simplify(X, SX).
simplify(X + X, 2 * SX) :- simplify(X, SX).

simplify(X + Y, SX + SY) :- simplify(X, SX), simplify(Y, SY).
simplify(X * Y, SX * SY) :- simplify(X, SX), simplify(Y, SY).

simplify(X, X).
