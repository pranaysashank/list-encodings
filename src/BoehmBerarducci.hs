
{-# LANGUAGE RankNTypes #-}

module BoehmBerarducci where

-- Oleg mentions that these are often wrongfully referred to
-- as Church encodings.
-- https://mail.haskell.org/pipermail/haskell-cafe/2012-September/103493.html

-- BoehmBerarducci encoded list
newtype List a = List
    { unList :: forall r. (a -> r -> r) -> r -> r
    }

{-# INLINE nil #-}
nil :: List a
nil = List $ \_ z -> z

{-# INLINE yield #-}
yield :: a -> List a
yield a = List $ \f z -> f a z

{-# INLINE cons #-}
cons :: a -> List a -> List a
cons x xs = List $ \f z -> f x (unList xs f z)

{-# INLINE uncons #-}
uncons :: List a -> Maybe (a, List a)
uncons xs =
    unList
        xs
        (\a r ->
             Just
                 ( a
                 , case r of
                       Nothing -> nil
                       Just (s, t) -> cons s t))
        Nothing

-- https://stackoverflow.com/questions/50765349/about-church-encoded-lists-in-haskell
{-# INLINE tail #-}
tail :: List a -> List a
tail xs = List $ \f z -> unList xs (\h t g -> g h (t f)) (\_ -> z) (\_ -> id)

{-# INLINE foldr #-}
foldr :: (a -> r -> r) -> r -> List a -> r
foldr f z xs = unList xs f z

{-# INLINE unfoldr #-}
unfoldr :: (s -> Maybe (a, s)) -> s -> List a
unfoldr step seed =
    List $ \f z ->
        let go s =
                case step s of
                    Just (a, b) -> f a (go b)
                    Nothing -> z
        in go seed

{-# INLINE append #-}
append :: List a -> List a -> List a
append xs ys = List $ \f z -> unList xs f (unList ys f z)

{-# INLINE toList #-}
toList :: List a -> [a]
toList xs = unList xs (:) []
