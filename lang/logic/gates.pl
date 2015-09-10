resistor(r1, power, n1).
resistor(r2, power, n2).

transistor(t1, n2, ground, n1).
transistor(t2, n3, n4, n2).
transistor(t3, n5, ground, n4).

inverter(inv(T, R), In, Out) :-
    transistor(T, In, ground, Out),
    resistor(R, power, Out).

nand_gate(nand(T1, T2, R), In1, In2, Out) :-
    transistor(T1, In1, X, Out),
    transistor(T2, In2, ground, X),
    resistor(R, power, Out).

and_gate(and(N, I), In1, In2, Out) :-
    nand_gate(N, In1, In2, X),
    inverter(I, X, Out).
