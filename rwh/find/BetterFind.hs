module BetterFild (betterFind) where

import Control.Exception (bracket)
import RecursiveContents (getRecursiveContents)

type Predicate = InfoP Bool

getFileSize :: FilePath -> Maybe (IO Integer)

betterFind :: Predicate -> FilePath ->IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)
    
getFileSize path = handle (\_ -> return Nothing) $
     bracket (openFile path ReadMode) hClose $  \h -> do
        size <- hFileSize h
        return (Just size)
    

type InfoP a = FilePath
             -> Permissions
             -> Maybe Integer
             -> ClockTime
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: Eq a => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: InfoP Bool
greaterP = liftP (>) 
lesserP = liftP (<)

liftP2 :: (a- > b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' q f k w x y z = f w x y z `q` constP k w x y z 

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w 

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP`(128 * 1024))


(==?) = equalP
(&&?) = andP
(||?) = orP

infix 4 ==?
infixr 3 &&?
infix  4 ||?

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP  >? (128 * 1024))
myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? (128 * 1024)


