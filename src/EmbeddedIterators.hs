{-# LANGUAGE RankNTypes, KindSignatures #-}

module EmbeddedIterators where

newtype List a = List
    { unList :: forall r. (Int -> a -> List a -> r) -> r -> r
    }

{-# INLINE getLen #-}
getLen :: List a -> Int
getLen m = unList m (\e _ _ -> e) 0

{-# INLINE nil #-}
nil :: List a
nil = List $ \_ stp -> stp

{-# INLINE yield #-}
yield :: a -> List a
yield x = List $ \yld _ -> yld 1 x nil

{-# INLINE cons #-}
cons :: a -> List a -> List a
cons x m = List $ \yld _ -> yld (1 + getLen m) x m

{-# INLINE uncons #-}
uncons :: List a -> Maybe (a, List a)
uncons xs = unList xs (\_ x r -> Just (x, r)) Nothing

{-# INLINE tail #-}
tail :: List a -> List a
tail xs = unList xs (\_ _ r -> r) nil

{-# INLINE toList #-}
toList :: List a -> [a]
toList m = go m
  where
    go m1 = let stop = []
                yieldk _ a r = (a : go r)
            in unList m1 yieldk stop

{-# INLINE unfoldr #-}
unfoldr :: (s -> Maybe (a, s)) -> s -> List a
unfoldr step seed = go 0 seed
  where
    go i s =
        List $ \f z ->
            case step s of
                Just (a, b) -> f (i+1) a (go (i+1) b)
                Nothing -> z

{-# INLINE append #-}
append :: List a -> List a -> List a
append la lb = go la
  where
    go m1 =
        List $ \f z ->
            unList
                m1
                (\_ a r -> let lp = go r
                           in f (1 + getLen lp) a lp)
                (unList lb f z)
          --(unList m1) (\_ a lp -> cons a (go lp)) lb --nil
