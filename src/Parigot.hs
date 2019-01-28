{-# LANGUAGE RankNTypes #-}

module Parigot where

-- Parigot
newtype List a = List
    { unList :: forall r. (a -> List a -> r -> r) -> r -> r
    }

{-# INLINE nil #-}
nil :: List a
nil = List $ \_ z -> z

{-# INLINE yield #-}
yield :: a -> List a
yield a = List $ \f z -> f a nil z

{-# INLINE cons #-}
cons :: a -> List a -> List a
cons x xs = List $ \f z -> f x xs (unList xs f z)

{-# INLINE uncons #-}
uncons :: List a -> Maybe (a, List a)
uncons xs = unList xs (\x r _ -> Just (x, r)) Nothing

{-# INLINE tail #-}
tail :: List a -> List a
tail xs = unList xs (\_ r _ -> r) nil

{-# INLINE foldr #-}
foldr :: (a -> r -> r) -> r -> List a -> r
foldr f z xs = unList xs (\a _ -> f a) z

{-# INLINE unfoldr #-}
unfoldr :: (s -> Maybe (a, s)) -> s -> List a
unfoldr step seed = go seed
  where
    go s =
        List $ \f z ->
            case step s of
                Just (a, b) -> f a (go b) (unList (go b) f z)
                Nothing -> z

{-# INLINE append #-}
append :: List a -> List a -> List a
append xs ys =
    List $ \f z -> unList xs (\a xs' -> f a (xs' `append` ys)) (unList ys f z)

{-{-# INLINE fromList #-}
fromList :: [a] -> List a
fromList = foldr cons nil-}

{-# INLINE toList #-}
toList :: List a -> [a]
toList xs = unList xs (\a _ -> (a:)) []
