{-# LANGUAGE RankNTypes #-}

module Scott where

import Prelude hiding (foldr)

-- Scott
newtype List a = List
    { unList :: forall r. (a -> List a -> r) -> r -> r
    }

{-# INLINE nil #-}
nil :: List a
nil = List $ \_ z -> z

{-# INLINE yield #-}
yield :: a -> List a
yield a = List $ \f _ -> f a nil

{-# INLINE cons #-}
cons :: a -> List a -> List a
cons x xs = List $ \f _ -> f x xs

{-# INLINE uncons #-}
uncons :: List a -> Maybe (a, List a)
uncons xs = unList xs (\x r -> Just (x, r)) Nothing

{-# INLINE tail #-}
tail :: List a -> List a
tail xs = unList xs (\_ r -> r) nil

{-# INLINE foldr #-}
foldr :: (a -> r -> r) -> r -> List a -> r
foldr f z xs = go xs
  where
    go m = unList m (\a r -> f a (go r)) z

{-# INLINE unfoldr #-}
unfoldr :: (s -> Maybe (a, s)) -> s -> List a
unfoldr step seed = go seed
  where
    go s =
        List $ \f z ->
            case step s of
                Just (a, b) -> f a (go b)
                Nothing -> z

{-# INLINE append #-}
append :: List a -> List a -> List a
append xs ys = go xs
  where
    go m1 = List $ \f z -> unList m1 (\a r -> f a (go r)) (unList ys f z)

{-{-# INLINE fromList #-}
fromList :: [a] -> List a
fromList = foldr cons nil-}

{-# INLINE toList #-}
toList :: List a -> [a]
toList = foldr (:) []
