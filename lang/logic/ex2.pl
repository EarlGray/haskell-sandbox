above(Block1, Block2) :- on(Block1, Block2).
above(Block1, Block2) :- on(Block1, Block), above(Block, Block2).
