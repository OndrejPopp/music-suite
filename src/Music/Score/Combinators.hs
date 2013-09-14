
{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstraintKinds #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Combinators for manipulating scores and related structures.
--
-------------------------------------------------------------------------------------

module Music.Score.Combinators (
        -- * Rests
        rest,
        noteRest,
        removeRests,

        -- --- * Retrograde
        -- retrograde,
        
        --- * Truncation
        Slicable(..),
        before,
        after,
        slice,           
        
        -- * Maps and filters
        -- ** Events
        Mappable(..),
        filterEvents,
        mapEvents,
        mapFilterEvents,

        -- ** Map over phrases
        Phraseable(..),
        mapFirst,
        mapLast,
        mapPhrase,
        mapPhraseSingle,

        -- * Parts
        -- ** Extracting parts
        filterPart,
        extractParts,
        extractParts',
        
        -- ** Map over parts
        mapPart,
        mapParts,
        mapAllParts,
        modifyParts,

        -- ** Part composition
        (</>),
        moveParts,
        moveToPart,

        -- * Zippers
        apply,
        snapshot,
        applySingle,
        snapshotSingle,
  ) where

import Control.Monad
import Control.Monad.Plus
import Data.Semigroup
import Data.String
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Ratio
import Data.Pointed
import Data.Ord

import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Convert
import Music.Time

import qualified Data.List as List
import qualified Data.Foldable as Foldable

-------------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------------

-- |
-- Create a score containing a rest at time zero of duration one.
--
-- > Score (Maybe a)
--
rest            :: MonadPlus s => s (Maybe a)

-- |
-- Create a note or a rest. This is an alias for 'mfromMaybe' with a nicer reading.
--
-- This function uses the unit position (0, 1).
--
-- > Maybe a -> Score a
--
noteRest        :: MonadPlus s => Maybe a -> s a

-- |
-- Remove all rests from a score. This is an alias for 'mcatMaybes' with a nicer reading.
--
-- > Score (Maybe a) -> Score a
--
removeRests     :: MonadPlus s => s (Maybe a) -> s a

rest            = return Nothing
noteRest        = mfromMaybe
removeRests     = mcatMaybes



{-
-- |
-- Reverse a score around its middle point (TODO not correct documentation w.r.t to start).
--
-- > onset a    = onset (retrograde a)
-- > duration a = duration (retrograde a)
-- > offset a   = offset (retrograde a)
--
-- > Score a -> Score a
--
retrograde      :: (Performable a, Composable a, HasOnset a, Ord (Duration a)) =>
                a -> a

retrograde = startAt origin . (mapAll $ fmap g)
    where
        g (t,d,x) = (negateP (t .+^ d), d, x)
        negateP a = origin .-^ (a .-. origin)
-}
instance Reversible a => Reversible (Score a) where
    rev = fmap rev . withSameOnset (mapAll $ fmap g)
        where
            g (t,d,x) = (negateP (t .+^ d), d, x)
            negateP a = origin .-^ (a .-. origin)

--------------------------------------------------------------------------------
-- Mapping
--------------------------------------------------------------------------------

type Mappable a b = (Performable a, Composable b, Duration a ~ Duration b)

-- |
-- Map over the events in a score.
--
-- > (Time -> Duration -> a -> b) -> Score a -> Score b
--
mapEvents       :: Mappable a b =>
                (Time a -> Duration a -> Event a -> Event b) -> a -> b

-- |
-- Filter the events in a score.
--
-- > (Time -> Duration -> a -> Bool) -> Score a -> Score b
--
filterEvents   :: (Performable a, Composable a) =>
                (Time a -> Duration a -> Event a -> Bool) -> a -> a

-- |
-- Efficient combination of 'mapEvents' and 'filterEvents'.
--
-- > (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
--
mapFilterEvents :: Mappable a b =>
                (Time a -> Duration a -> Event a -> Maybe (Event b)) -> a -> b

-- |
-- The same as 'mfilter' but with a non-monadic type.
--
-- > (a -> Bool) -> Score a -> Score a
--
filter_         :: (Performable a, Composable a) => (Event a -> Bool)    -> a -> a
map_            :: Mappable a b => (Event a -> Event b) -> a -> b

-- |
-- Map over all events in a score.
--
-- > ([(Time, Duration, a)] -> [(Time, Duration, b)]) -> Score a -> Score b
--
mapAll          :: (Mappable a b, t ~ Time a, d ~ Duration a, e ~ Event a, e' ~ Event b) => 
                ([(t, d, e)] -> [(t, d, e')]) -> a -> b

mapEvents f                 = mapAll $ fmap (third' f)
filterEvents f              = mapFilterEvents (partial3 f)
mapFilterEvents f           = mapAll $ mcatMaybes . fmap (unM . third' f)
    where
        unM (a,b,Nothing) = Nothing
        unM (a,b,Just c)  = Just (a,b,c)
filter_ p = filterEvents (\t d x -> p x)
map_ f    = mapEvents (\t d x -> f x)
mapAll f                    = compose . f . perform


type Slicable a = (Ord (Duration a), AdditiveGroup (Duration a), Performable a, Composable a)

-- |
-- Return a score containing only the notes whose offset falls before the given duration.
--
-- > Time -> Score a -> Score a
--
before          :: Slicable a =>
                Time a -> a -> a

-- |
-- Return a score containing only the notes whose onset falls after given duration.
--
-- > Time -> Score a -> Score a
--
after           :: Slicable a =>
                Time a -> a -> a

-- |
-- Return a score containing only the notes whose onset and offset falls between the given durations.
--
-- > Time -> Time -> Score a -> Score a
--
slice           :: Slicable a =>
                Time a -> Time a -> a -> a

after  a                    = filterEvents (\t d _ -> a <= t)
before b                    = filterEvents (\t d _ -> t .+^ d <= b) 
slice  a b                  = filterEvents (\t d _ -> a <= t && t .+^ d <= b)


type Phraseable a b = (Mappable a b, Composable a, HasPart' (Event a))

-- |
-- Map over the first, and remaining notes in each part.
--
-- If a part has only one notes, the first function is applied.
-- If a part has no notes, the given score is returned unchanged.
--
-- > (a -> b) -> (a -> b) -> Score a -> Score b
--
mapFirst    :: (Phraseable a b, e ~ Event a, e' ~ Event b) =>
            (e -> e') -> (e -> e') -> a -> b

-- |
-- Map over the last, and preceding notes in each part.
--
-- If a part has only one notes, the first function is applied.
-- If a part has no notes, the given score is returned unchanged.
--
-- > (a -> b) -> (a -> b) -> Score a -> Score b
--
mapLast     :: (Phraseable a b, e ~ Event a, e' ~ Event b) =>
            (e -> e') -> (e -> e') -> a -> b

-- |
-- Map over the first, middle and last note in each part.
--
-- If a part has fewer than three notes the first takes precedence over the last,
-- and last takes precedence over the middle.
--
-- > (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
--
mapPhrase       :: (Phraseable a b, e ~ Event a, e' ~ Event b) =>
                (e -> e') -> (e -> e') -> (e -> e') -> a -> b

-- |
-- Equivalent to 'mapPhrase' for single-part scores.
--
-- Fails if the score contains overlapping events.
--
-- > (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
--
mapPhraseSingle :: (Mappable a b, e ~ Event a, e' ~ Event b) =>
                (e -> e') -> (e -> e') -> (e -> e') -> a -> b


mapFirst f g                = mapPhrase f g g
mapLast f g                 = mapPhrase g g f
mapPhrase f g h             = mapAllParts (fmap $ mapPhraseSingle f g h)
mapPhraseSingle f g h       = mapAll (mapFirstMiddleLast (third f) (third g) (third h))


--------------------------------------------------------------------------------
-- Parts
--------------------------------------------------------------------------------

-- |
-- Filter a score to include only those events whose parts match a given predicate.
--
-- > (Part -> Bool) -> Score a -> Score a
--
filterPart :: (Mappable a a, Event a ~ e, HasPart' e) => (Part e -> Bool) -> a -> a
filterPart p = filter_ (p . getPart)

-- |
-- Extract parts from the a score.
--
-- The parts are returned in the order defined the associated 'Ord' instance part type.
-- You can recompose the score with 'mconcat', i.e.
--
-- > mconcat . extractParts = id
--
-- > Score a -> [Score a]
--
extractParts :: (HasPart' e, Mappable a a, e ~ Event a) => a -> [a]
extractParts a = fmap (\p -> filterPart (== p) a) (getParts a)
        
-- |
-- Extract parts from the a score and include the part name.
--
-- The parts are returned in the order defined the associated 'Ord' instance part type.
--
-- Simple type
--
-- > Score a -> [(Part a, Score a)]
--
extractParts' :: (HasPart' e, Mappable a a, e ~ Event a) => a -> [(Part e, a)]
extractParts' a = fmap (\p -> (p, filterPart (== p) a)) (getParts a)


-- |
-- Map over a specific part in the given score.
--
-- > Part -> (Score a -> Score a) -> Score a -> Score a
--
mapPart         :: (Enum a, Mappable b b, HasPart' e, e ~ Event b) => 
                a -> (b -> b) -> b -> b

-- |
-- Map over all parts in the given score.
--
-- > (Score a -> Score a) -> Score a -> Score a
--
mapParts        :: (Monoid' b, Mappable a a, HasPart' e, e ~ Event a) => 
                (a -> b) -> a -> b

-- |
-- Map over all parts in the given score.
--
-- > ([Score a] -> [Score a]) -> Score a -> Score a
--
mapAllParts     :: (Monoid' b, Mappable a a, HasPart' e, e ~ Event a) => 
                 ([a] -> [b]) -> a -> b


mapPart n f     = mapAllParts (zipWith ($) (replicate (fromEnum n) id ++ [f] ++ repeat id))
mapParts f      = mapAllParts (fmap f)
mapAllParts f   = mconcat . f . extractParts

-- |
-- Modify all parts in the given score.
--
-- > (Part -> Part) -> Score a -> Score a
--
modifyParts :: (Mappable a a, HasPart e, e ~ Event a) => (Part e -> Part e) -> a -> a
modifyParts n = map_ (modifyPart n)



--------------------------------------------------------------------------------
-- Part composition
--------------------------------------------------------------------------------

infixr 6 </>

-- |
-- Similar to '<>', but increases parts in the second part to prevent collision.
--
(</>) :: (Enum (Part e), Mappable a a, HasPart' e, e ~ Event a) => a -> a -> a
a </> b = a <> moveParts offset b
    where
        -- max voice in a + 1
        offset = succ $ maximum' 0 $ fmap fromEnum $ getParts a

-- |
-- Move down one voice (all parts).
--
moveParts       :: (Enum (Part e), Integral b, Mappable a a, HasPart e, e ~ Event a) =>
                b -> a -> a
moveParts x = modifyParts (successor x)

-- |
-- Move top-part to the specific voice (other parts follow).
--
moveToPart :: (Enum (Part e), Enum b, Mappable a a, HasPart e, e ~ Event a) => b -> a -> a
moveToPart v = moveParts (fromEnum v)

successor :: (Integral b, Enum a) => b -> a -> a
successor n | n <  0 = (!! fromIntegral (abs n)) . iterate pred
            | n >= 0 = (!! fromIntegral n)       . iterate succ

maximum' :: (Ord a, Foldable t) => a -> t a -> a
maximum' z = option z getMax . foldMap (Option . Just . Max)

minimum' :: (Ord a, Foldable t) => a -> t a -> a
minimum' z = option z getMin . foldMap (Option . Just . Min)



                                                                 
-------------------------------------------------------------------------------------
-- Zippers

-- |
-- Apply a time-varying function to all events in score.
--
apply :: HasPart' a => Voice (Score a -> Score b) -> Score a -> Score b
apply x = mapAllParts (fmap $ applySingle x)

-- |
-- Get all notes that start during a given note.
--
snapshot :: HasPart' a => Score b -> Score a -> Score (b, Score a)
snapshot x = mapAllParts (fmap $ snapshotSingle x)


snapshot2
    :: (
        Mappable a b,
        Mappable b c,
        Ord (Duration c),
        
        HasPart' (Event b),
        Event c ~ (Event a, b)
        ) =>
       a -> b -> c
    
snapshot2 x = mapAllParts (fmap $ snapshotSingle2 x)



-- |
-- Apply a time-varying function to all events in score.
--
applySingle :: Voice (Score a -> Score b) -> Score a -> Score b
applySingle fs as = notJoin $ fmap (uncurry ($)) $ sample fs $ as
    where
        -- This is not join; we simply concatenate all inner scores in parallel
        notJoin   = mconcat . performValues
        sample fs = snapshotSingle (voiceToScore fs)


-- |
-- Get all notes that start during a given note.
--
snapshotSingle :: Score a -> Score b -> Score (a, Score b)
snapshotSingle = snapshotSingleWith (,)


snapshotSingle2     :: (Mappable a b, Mappable b c, d ~ Duration b, Ord d, Event c ~ (Event a, b)) => 
                       a -> b -> c
snapshotSingle2 = snapshotSingleWith (,)

-- snapshotSingleWith :: (a -> Score b -> c) -> Score a -> Score b -> Score c
snapshotSingleWith  :: (Mappable a b, Mappable b c, d ~ Duration b, Ord d) =>
                       (Event a -> b -> Event c) -> a -> b -> c
    
snapshotSingleWith g as bs = mapEvents ( \t d a -> g a (onsetIn t d bs) ) as


-- |
-- Filter out events that has its onset in the given time interval (inclusive start).
-- For example, onset in 1 2 filters events such that (1 <= onset x < 3)
--
onsetIn :: (Performable a, Composable a, Ord (Duration a)) => Time a -> Duration a -> a -> a
onsetIn a b = mapAll $ filterOnce (\(t,d,x) -> a <= t && t < a .+^ b)
    -- Note: filterOnce is more lazy than mfilter but depends on the events being sorted



--------------------------------------------------------------------------------

-- |
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
--
mapFirstMiddleLast :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFirstMiddleLast f g h = go
    where
        go []    = []
        go [a]   = [f a]
        go [a,b] = [f a, h b]
        go xs    = [f $ head xs]          ++ 
                   map g (tail $ init xs) ++ 
                   [h $ last xs]

-- |
-- Extract the first consecutive sublist for which the predicate returns true, or
-- the empty list if no such sublist exists.
filterOnce :: (a -> Bool) -> [a] -> [a]
filterOnce p = List.takeWhile p . List.dropWhile (not . p)


delay' t = delay (t .-. zeroV)

fst3 (t, d, x) = t
trd3 (a,b,c) = c

third f (a,b,c) = (a,b,f c)
third' f (a,b,c) = (a,b,f a b c)

rotl []     = []
rotl (x:xs) = xs ++ [x]

rotr [] = []
rotr xs = last xs : init xs


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 = curry . curry . (. trip)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = (. untrip) . uncurry . uncurry

untrip (a,b,c) = ((a,b),c)
trip ((a,b),c) = (a,b,c)

partial2 :: (a -> b -> Bool)      -> a -> b -> Maybe b
partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
partial2 f = curry  (fmap snd  . partial (uncurry f))
partial3 f = curry3 (fmap trd3 . partial (uncurry3 f))

rotated :: Int -> [a] -> [a]
rotated = go
    where
        go n as 
            | n >= 0 = iterate rotr as !! n
            | n <  0 = iterate rotl as !! abs n

