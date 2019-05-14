
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides overloaded pitch literals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Literal.Pitch (

        -- * IsPitch class
        IsPitch(..),

        -- * Literal values

        -- ** Four octaves up
        cs'''', ds'''', es'''', fs'''', gs'''', as'''', bs'''',
        c'''' , d'''' , e'''' , f'''' , g'''' , a'''' , b'''' ,
        cb'''', db'''', eb'''', fb'''', gb'''', ab'''', bb'''',

        -- ** Three octaves up
        cs''', ds''', es''', fs''', gs''', as''', bs''',
        c''' , d''' , e''' , f''' , g''' , a''' , b''' ,
        cb''', db''', eb''', fb''', gb''', ab''', bb''',

        -- ** Two octaves up
        cs'', ds'', es'', fs'', gs'', as'', bs'',
        c'' , d'' , e'' , f'' , g'' , a'' , b'' ,
        cb'', db'', eb'', fb'', gb'', ab'', bb'',

        -- ** One octave up
        cs' , ds' , es' , fs' , gs' , as' , bs' ,
        c'  , d'  , e'  , f'  , g'  , a'  , b'  ,
        cb' , db' , eb' , fb' , gb' , ab' , bb' ,

        -- ** Standard octave
        cs  , ds  , es  , fs  , gs  , as  , bs  ,
        c   , d   , e   , f   , g   , a   , b   ,
        cb  , db  , eb  , fb  , gb  , ab  , bb  ,

        -- ** One octave down
        cs_ , ds_ , es_ , fs_ , gs_ , as_ , bs_ ,
        c_  , d_  , e_  , f_  , g_  , a_  , b_  ,
        cb_ , db_ , eb_ , fb_ , gb_ , ab_ , bb_ ,

        -- ** Two octaves down
        cs__, ds__, es__, fs__, gs__, as__, bs__,
        c__ , d__ , e__ , f__ , g__ , a__ , b__ ,
        cb__, db__, eb__, fb__, gb__, ab__, bb__,

        -- ** Three octaves down
        cs___, ds___, es___, fs___, gs___, as___, bs___,
        c___ , d___ , e___ , f___ , g___ , a___ , b___ ,
        cb___, db___, eb___, fb___, gb___, ab___, bb___,

        -- ** Four octaves down
        cs____, ds____, es____, fs____, gs____, as____, bs____,
        c____ , d____ , e____ , f____ , g____ , a____ , b____ ,
        cb____, db____, eb____, fb____, gb____, ab____, bb____,

  ) where

import           Control.Applicative
import           Data.Fixed
import           Data.Int
import           Data.Ratio
import           Data.Semigroup
import           Data.Word
import Data.AffineSpace ((.-.))

import Music.Pitch.Common.Types

-- Pitch literal, defined as @(class, alteration, octave)@, where
--
--     * @class@      is a pitch class number in @[0..6]@, starting from C.
--
--     * @alteration@ is the number of semitones, i.e. 0 is natural, 1 for sharp 2 for double sharp, -1 for flat and -2 for double flat.
--       Alteration is in 'Maybe' because some pitch representations differ between explicit and explicit accidentals, i.e. a diatonic
--       pitch type may assume @(0,Nothing,...)@ to mean C sharp rather than C.
--
--     * @octave@     is octave number in scientific pitch notation - 4.
--
-- Middle C is represented by the pitch literal @(0, Nothing, 0)@.
--

class IsPitch a where
    fromPitch :: Pitch -> a

instance IsPitch a => IsPitch (Maybe a) where
    fromPitch = pure . fromPitch

instance IsPitch a => IsPitch (First a) where
    fromPitch = pure . fromPitch

instance IsPitch a => IsPitch (Last a) where
    fromPitch = pure . fromPitch

instance IsPitch a => IsPitch [a] where
    fromPitch = pure . fromPitch

instance (Monoid b, IsPitch a) => IsPitch (b, a) where
    fromPitch = pure . fromPitch

-- TODO clean by inlining this whole thing or similar
mkPitch :: (Int, Int, Int) -> Pitch
mkPitch (pc, sem, oct) = Pitch $ mkInterval' sem (oct * 7 + pc)
  where
    mkInterval' diff diatonic = Interval (diatonicToChromatic (fromIntegral diatonic) + fromIntegral diff, fromIntegral diatonic)

    diatonicToChromatic :: DiatonicSteps -> ChromaticSteps
    diatonicToChromatic d = fromIntegral $ (octaves*12) + go restDia
        where
            -- restDia is always in [0..6]
            (octaves, restDia) = fromIntegral d `divMod` 7
            go = ([0,2,4,5,7,9,11] !!)


cs''''    = fromPitch $ mkPitch (0, 1, 4)
ds''''    = fromPitch $ mkPitch (1, 1, 4)
es''''    = fromPitch $ mkPitch (2, 1, 4)
fs''''    = fromPitch $ mkPitch (3, 1, 4)
gs''''    = fromPitch $ mkPitch (4, 1, 4)
as''''    = fromPitch $ mkPitch (5, 1, 4)
bs''''    = fromPitch $ mkPitch (6, 1, 4)

c''''     = fromPitch $ mkPitch (0, 0, 4)
d''''     = fromPitch $ mkPitch (1, 0, 4)
e''''     = fromPitch $ mkPitch (2, 0, 4)
f''''     = fromPitch $ mkPitch (3, 0, 4)
g''''     = fromPitch $ mkPitch (4, 0, 4)
a''''     = fromPitch $ mkPitch (5, 0, 4)
b''''     = fromPitch $ mkPitch (6, 0, 4)

cb''''    = fromPitch $ mkPitch (0, (-1), 4)
db''''    = fromPitch $ mkPitch (1, (-1), 4)
eb''''    = fromPitch $ mkPitch (2, (-1), 4)
fb''''    = fromPitch $ mkPitch (3, (-1), 4)
gb''''    = fromPitch $ mkPitch (4, (-1), 4)
ab''''    = fromPitch $ mkPitch (5, (-1), 4)
bb''''    = fromPitch $ mkPitch (6, (-1), 4)

cs'''     = fromPitch $ mkPitch (0, 1, 3)
ds'''     = fromPitch $ mkPitch (1, 1, 3)
es'''     = fromPitch $ mkPitch (2, 1, 3)
fs'''     = fromPitch $ mkPitch (3, 1, 3)
gs'''     = fromPitch $ mkPitch (4, 1, 3)
as'''     = fromPitch $ mkPitch (5, 1, 3)
bs'''     = fromPitch $ mkPitch (6, 1, 3)

c'''      = fromPitch $ mkPitch (0, 0, 3)
d'''      = fromPitch $ mkPitch (1, 0, 3)
e'''      = fromPitch $ mkPitch (2, 0, 3)
f'''      = fromPitch $ mkPitch (3, 0, 3)
g'''      = fromPitch $ mkPitch (4, 0, 3)
a'''      = fromPitch $ mkPitch (5, 0, 3)
b'''      = fromPitch $ mkPitch (6, 0, 3)

cb'''     = fromPitch $ mkPitch (0, (-1), 3)
db'''     = fromPitch $ mkPitch (1, (-1), 3)
eb'''     = fromPitch $ mkPitch (2, (-1), 3)
fb'''     = fromPitch $ mkPitch (3, (-1), 3)
gb'''     = fromPitch $ mkPitch (4, (-1), 3)
ab'''     = fromPitch $ mkPitch (5, (-1), 3)
bb'''     = fromPitch $ mkPitch (6, (-1), 3)

cs''      = fromPitch $ mkPitch (0, 1, 2)
ds''      = fromPitch $ mkPitch (1, 1, 2)
es''      = fromPitch $ mkPitch (2, 1, 2)
fs''      = fromPitch $ mkPitch (3, 1, 2)
gs''      = fromPitch $ mkPitch (4, 1, 2)
as''      = fromPitch $ mkPitch (5, 1, 2)
bs''      = fromPitch $ mkPitch (6, 1, 2)

c''       = fromPitch $ mkPitch (0, 0, 2)
d''       = fromPitch $ mkPitch (1, 0, 2)
e''       = fromPitch $ mkPitch (2, 0, 2)
f''       = fromPitch $ mkPitch (3, 0, 2)
g''       = fromPitch $ mkPitch (4, 0, 2)
a''       = fromPitch $ mkPitch (5, 0, 2)
b''       = fromPitch $ mkPitch (6, 0, 2)

cb''      = fromPitch $ mkPitch (0, (-1), 2)
db''      = fromPitch $ mkPitch (1, (-1), 2)
eb''      = fromPitch $ mkPitch (2, (-1), 2)
fb''      = fromPitch $ mkPitch (3, (-1), 2)
gb''      = fromPitch $ mkPitch (4, (-1), 2)
ab''      = fromPitch $ mkPitch (5, (-1), 2)
bb''      = fromPitch $ mkPitch (6, (-1), 2)

cs'       = fromPitch $ mkPitch (0, 1, 1)
ds'       = fromPitch $ mkPitch (1, 1, 1)
es'       = fromPitch $ mkPitch (2, 1, 1)
fs'       = fromPitch $ mkPitch (3, 1, 1)
gs'       = fromPitch $ mkPitch (4, 1, 1)
as'       = fromPitch $ mkPitch (5, 1, 1)
bs'       = fromPitch $ mkPitch (6, 1, 1)

c'        = fromPitch $ mkPitch (0, 0, 1)
d'        = fromPitch $ mkPitch (1, 0, 1)
e'        = fromPitch $ mkPitch (2, 0, 1)
f'        = fromPitch $ mkPitch (3, 0, 1)
g'        = fromPitch $ mkPitch (4, 0, 1)
a'        = fromPitch $ mkPitch (5, 0, 1)
b'        = fromPitch $ mkPitch (6, 0, 1)

cb'       = fromPitch $ mkPitch (0, (-1), 1)
db'       = fromPitch $ mkPitch (1, (-1), 1)
eb'       = fromPitch $ mkPitch (2, (-1), 1)
fb'       = fromPitch $ mkPitch (3, (-1), 1)
gb'       = fromPitch $ mkPitch (4, (-1), 1)
ab'       = fromPitch $ mkPitch (5, (-1), 1)
bb'       = fromPitch $ mkPitch (6, (-1), 1)

cs        = fromPitch $ mkPitch (0, 1, 0)
ds        = fromPitch $ mkPitch (1, 1, 0)
es        = fromPitch $ mkPitch (2, 1, 0)
fs        = fromPitch $ mkPitch (3, 1, 0)
gs        = fromPitch $ mkPitch (4, 1, 0)
as        = fromPitch $ mkPitch (5, 1, 0)
bs        = fromPitch $ mkPitch (6, 1, 0)

c         = fromPitch $ mkPitch (0, 0, 0)
d         = fromPitch $ mkPitch (1, 0, 0)
e         = fromPitch $ mkPitch (2, 0, 0)
f         = fromPitch $ mkPitch (3, 0, 0)
g         = fromPitch $ mkPitch (4, 0, 0)
a         = fromPitch $ mkPitch (5, 0, 0)
b         = fromPitch $ mkPitch (6, 0, 0)

cb        = fromPitch $ mkPitch (0, (-1), 0)
db        = fromPitch $ mkPitch (1, (-1), 0)
eb        = fromPitch $ mkPitch (2, (-1), 0)
fb        = fromPitch $ mkPitch (3, (-1), 0)
gb        = fromPitch $ mkPitch (4, (-1), 0)
ab        = fromPitch $ mkPitch (5, (-1), 0)
bb        = fromPitch $ mkPitch (6, (-1), 0)

cs_       = fromPitch $ mkPitch (0, 1, -1)
ds_       = fromPitch $ mkPitch (1, 1, -1)
es_       = fromPitch $ mkPitch (2, 1, -1)
fs_       = fromPitch $ mkPitch (3, 1, -1)
gs_       = fromPitch $ mkPitch (4, 1, -1)
as_       = fromPitch $ mkPitch (5, 1, -1)
bs_       = fromPitch $ mkPitch (6, 1, -1)

c_        = fromPitch $ mkPitch (0, 0, -1)
d_        = fromPitch $ mkPitch (1, 0, -1)
e_        = fromPitch $ mkPitch (2, 0, -1)
f_        = fromPitch $ mkPitch (3, 0, -1)
g_        = fromPitch $ mkPitch (4, 0, -1)
a_        = fromPitch $ mkPitch (5, 0, -1)
b_        = fromPitch $ mkPitch (6, 0, -1)

cb_       = fromPitch $ mkPitch (0, (-1), -1)
db_       = fromPitch $ mkPitch (1, (-1), -1)
eb_       = fromPitch $ mkPitch (2, (-1), -1)
fb_       = fromPitch $ mkPitch (3, (-1), -1)
gb_       = fromPitch $ mkPitch (4, (-1), -1)
ab_       = fromPitch $ mkPitch (5, (-1), -1)
bb_       = fromPitch $ mkPitch (6, (-1), -1)

cs__      = fromPitch $ mkPitch (0, 1, -2)
ds__      = fromPitch $ mkPitch (1, 1, -2)
es__      = fromPitch $ mkPitch (2, 1, -2)
fs__      = fromPitch $ mkPitch (3, 1, -2)
gs__      = fromPitch $ mkPitch (4, 1, -2)
as__      = fromPitch $ mkPitch (5, 1, -2)
bs__      = fromPitch $ mkPitch (6, 1, -2)

c__       = fromPitch $ mkPitch (0, 0, -2)
d__       = fromPitch $ mkPitch (1, 0, -2)
e__       = fromPitch $ mkPitch (2, 0, -2)
f__       = fromPitch $ mkPitch (3, 0, -2)
g__       = fromPitch $ mkPitch (4, 0, -2)
a__       = fromPitch $ mkPitch (5, 0, -2)
b__       = fromPitch $ mkPitch (6, 0, -2)

cb__      = fromPitch $ mkPitch (0, (-1), -2)
db__      = fromPitch $ mkPitch (1, (-1), -2)
eb__      = fromPitch $ mkPitch (2, (-1), -2)
fb__      = fromPitch $ mkPitch (3, (-1), -2)
gb__      = fromPitch $ mkPitch (4, (-1), -2)
ab__      = fromPitch $ mkPitch (5, (-1), -2)
bb__      = fromPitch $ mkPitch (6, (-1), -2)

cs___     = fromPitch $ mkPitch (0, 1, -3)
ds___     = fromPitch $ mkPitch (1, 1, -3)
es___     = fromPitch $ mkPitch (2, 1, -3)
fs___     = fromPitch $ mkPitch (3, 1, -3)
gs___     = fromPitch $ mkPitch (4, 1, -3)
as___     = fromPitch $ mkPitch (5, 1, -3)
bs___     = fromPitch $ mkPitch (6, 1, -3)

c___      = fromPitch $ mkPitch (0, 0, -3)
d___      = fromPitch $ mkPitch (1, 0, -3)
e___      = fromPitch $ mkPitch (2, 0, -3)
f___      = fromPitch $ mkPitch (3, 0, -3)
g___      = fromPitch $ mkPitch (4, 0, -3)
a___      = fromPitch $ mkPitch (5, 0, -3)
b___      = fromPitch $ mkPitch (6, 0, -3)

cb___     = fromPitch $ mkPitch (0, (-1), -3)
db___     = fromPitch $ mkPitch (1, (-1), -3)
eb___     = fromPitch $ mkPitch (2, (-1), -3)
fb___     = fromPitch $ mkPitch (3, (-1), -3)
gb___     = fromPitch $ mkPitch (4, (-1), -3)
ab___     = fromPitch $ mkPitch (5, (-1), -3)
bb___     = fromPitch $ mkPitch (6, (-1), -3)

cs____    = fromPitch $ mkPitch (0, 1, -4)
ds____    = fromPitch $ mkPitch (1, 1, -4)
es____    = fromPitch $ mkPitch (2, 1, -4)
fs____    = fromPitch $ mkPitch (3, 1, -4)
gs____    = fromPitch $ mkPitch (4, 1, -4)
as____    = fromPitch $ mkPitch (5, 1, -4)
bs____    = fromPitch $ mkPitch (6, 1, -4)

c____     = fromPitch $ mkPitch (0, 0, -4)
d____     = fromPitch $ mkPitch (1, 0, -4)
e____     = fromPitch $ mkPitch (2, 0, -4)
f____     = fromPitch $ mkPitch (3, 0, -4)
g____     = fromPitch $ mkPitch (4, 0, -4)
a____     = fromPitch $ mkPitch (5, 0, -4)
b____     = fromPitch $ mkPitch (6, 0, -4)

cb____    = fromPitch $ mkPitch (0, (-1), -4)
db____    = fromPitch $ mkPitch (1, (-1), -4)
eb____    = fromPitch $ mkPitch (2, (-1), -4)
fb____    = fromPitch $ mkPitch (3, (-1), -4)
gb____    = fromPitch $ mkPitch (4, (-1), -4)
ab____    = fromPitch $ mkPitch (5, (-1), -4)
bb____    = fromPitch $ mkPitch (6, (-1), -4)

