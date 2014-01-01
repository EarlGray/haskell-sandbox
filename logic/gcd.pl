gcd(X, X, X).
gcd(X, Y, Out) :-
    X < Y,
    Z is Y - X,
    gcd(X, Z, Out).
gcd(X, Y, Out) :-
    Y < X,
    gcd(Y, X, Out).

takeout(A, [A|B], B).
takeout(A, [B|C], [B|D]) :-
    takeout(A,C,D).
