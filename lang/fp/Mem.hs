{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Mem (
    Mem(..),
    alter
) where

class (Eq (Ref r), Monad r) => Mem r where
    data Ref r
    type Val r
    
    ref   :: Val r -> r (Ref r)
    deref :: Ref r -> r (Val r)
    set   :: Ref r -> Val r -> r ()

alter :: Mem r => (Val r -> Val r) -> Ref r -> r ()
alter f r = do
    v <- deref r
    set r (f v)

