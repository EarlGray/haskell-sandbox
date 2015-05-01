father(terach,abraham).
father(terach,nachor).
father(terach,haran).
father(abraham,isaac).
father(haran,lot).
father(haran,milcah).
father(haran,yiscah).

mother(sarah,isaac).

male(terach).
male(abraham).
male(nachor).
male(haran).
male(isaac).
male(lot).

female(sarah).
female(milcah).
female(yiscah).

% disjunction by alternative rules:
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).

son(X,Y) :- parent(Y,X), male(X).
grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
grandfather(X,Y) :- grandparent(X,Y),male(X).

likes(X,pomegranates). %% X is every biblical character mentioned above.
