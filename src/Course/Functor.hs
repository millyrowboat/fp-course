{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) :: (a -> b) -> f a -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance Functor ExactlyOne where
  (<$>) ::
    (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<$>) =
    error "todo: Course.Functor (<$>)#instance ExactlyOne"

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  (<$>) _ Nil = Nil
-- Cheat: (<$>) = map
  (<$>) f (h:.t) = f h:.(f <$> t)

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  (<$>) = mapOptional

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) :: (a -> b) -> (t -> a) -> (t -> b)
--Fast way:  (<$>) = (.)
  (<$>) = \a2b t2a t -> a2b (t2a t)

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> x <$ Full q == Full x
(<$) :: Functor f => a -> f b -> f a
(<$) =
  \a x -> (<$>) (\_ -> a) x
  -- (<$>) . const






-- ###################
-- Making a point: Exercises
-- anon1 :: a -> List b -> List b
-- anon1 = \a x -> map (\_ -> a) x

-- anon2 :: a -> List b -> List b
-- anon2 = \a x -> mapOptional (\_ -> a) x

--etcetc How do we remove repetition?
-- Use a map that works for all instances of Functor!
-- We write these so we don't have to repeat ourselves.
-- Just like writing a sort function for every kind of list

-- #####################






-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void :: Functor f => f a -> f ()
void =
  (<$) ()

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
