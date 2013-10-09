r n k=drop(9*rem n k)
main=mapM(\n->putStrLn$max(show n)$r n 3"Fizz"++r n 5"Buzz")[1..100]
