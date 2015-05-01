edge(a,b). edge(a,c). edge(b,d). edge(c,d). edge(d,e).
edge(f,g).

connected(Node, Node).
connected(Node1, Node2) :- edge(Node1, LinkNode), connected(LinkNode, Node2).
