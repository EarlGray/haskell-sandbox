binary_tree(void).
binary_tree(tree(_, Left, Right)) :-
    binary_tree(Left), binary_tree(Right).

tree_member(X, tree(X, _, _)).
tree_member(X, tree(_, Left, _))  :- tree_member(X, Left).
tree_member(X, tree(_, _, Right)) :- tree_member(X, Right).

isotree(void, void).
isotree(tree(X, L1, R1), tree(X, L2, R2)) :- 
    isotree(L1, L2), isotree(R1, R2).
isotree(tree(X, L1, R1), tree(X, L2, R2)) :- 
    isotree(L1, R2), isotree(R1, L2).


tree_subst(_, _, void, void).
tree_subst(X, Y, tree(N, L, R), tree(N1, L1, R1)) :-
    replace(X, Y, N, N1),
    tree_subst(X, Y, L, L1), tree_subst(X, Y, R, R1).

replace(X, Y, X, Y).
replace(X, _, Z, Z) :- not(X = Z).

preorder(void, []).
preorder(tree(X, L, R), [X|Preord]) :-
    preorder(L, Lpo), preorder(R, Rpo),
    append(Lpo, Rpo, Preord).

inorder(void, []).
inorder(tree(X, L, R), Ord) :- inorder(L, Lo), inorder(R, Ro), append(Lo, [X|Ro], Ord).

postorder(void, []).
postorder(tree(X, L, R), Ord) :-
    postorder(L, Lo), postorder(R, Ro),
    append(Lo, Ro, Po), append(Po, [X], Ord).

%% heapify(Tree, Heap).
% heap_adjust(X, HeapL, HeapR, Heap):
tree_gt(_, void).
tree_gt(X, tree(Y, _, _)) :- X >= Y.

heap_adjust(X, HeapL, HeapR, tree(X, HeapL, HeapR)) :-
    tree_gt(X, HeapL), tree_gt(X, HeapR).
heap_adjust(X, tree(X1, L, R), HeapR, tree(X1, HeapL, HeapR)) :-
    X < X1, heap_adjust(X, L, R, HeapL), tree_gt(X, HeapR).
heap_adjust(X, HeapL, tree(X1, L, R), tree(X1, HeapL, HeapR)) :-
    X < X1, tree_gt(X, HeapL), heap_adjust(X, L, R, HeapR).
heap_adjust(X, HeapL, tree(X1, L, R), Heap) :-
    X < X1, not(tree_gt(X, HeapL)),
    heapify(tree(X, L, R), HeapR),
    heap_adjust(X1, HeapL, HeapR, Heap).

heapify(void, void).
heapify(tree(X, L, R), Heap) :-
    heapify(L, HeapL), heapify(R, HeapR),
    heap_adjust(X, HeapL, HeapR, Heap).

