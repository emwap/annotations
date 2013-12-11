{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Annot where

import qualified Prelude as P

import Feldspar
import Feldspar.Vector
import Feldspar.Matrix ((***))

import Feldspar.Core.Constructs.SourceInfo (SourceInfo1(..))

class Annot a where
    annot :: String -> a -> a

instance Annot a where
    annot _ = id

instance (Annot b) => Annot (a -> b) where
    annot info f = annot info . f

instance (Type a) => Annot (Data a) where
    annot = sourceData . SourceInfo1

instance (Annot a) => Annot (Vector a) where
    annot _    Empty = Empty
    annot info vec   = annotVec info vec

annotVec :: Annot a => String -> Vector a -> Vector a
annotVec info (Indexed len ixf cont) =
    Indexed (annot (info P.++ " len") len) (annot (info P.++ " elem") ixf) (annot info cont)

copy, alloc, alloccopy :: Annot a => a -> a
copy      = annot "Copy"
alloc     = annot "Alloc"
alloccopy = annot "AllocCopy"

green, red :: Annot a => a -> a
green = annot "G"
red   = annot "R"

prog1 :: (Syntax a, Num a)
      => Vector a -> a
prog1 = sum

prog2 :: (Syntax a, Num a)
      => Vector a -> Vector a -> a
prog2 v1 v2 = sum (zipWith (*) (green v1) (green v2))

prog3 :: (Syntax b, Num b)
      => Vector b -> Vector b
prog3 v = forLoop 3 (alloc v) (copy (const $ map (+1)))

prog4 l = let v = indexed l id
          in zipWith (+) v (alloccopy $ force v)

prog5 l = let v = parallel l id
          in v `append` v

causalMap :: Syntax a => (Vector a -> a) -> Vector a -> Vector a
causalMap f = map (f . reverse) . inits

-- | FIR filter
fir :: Vector1 Float  -- ^ Coefficients
    -> Vector1 Float  -- ^ Input
    -> Vector1 Float
fir coeffs = causalMap (coeffs***)

---- { fft
dft2 :: Num a => a -> (a,a) -> (a,a)
dft2 w (x0,x1) = (x0+x1, (x0-x1)*w)

butterfly :: (Syntax a)
          => (a -> (a,a) -> (a,a))
          -> Vector a -> Vector a -> Vector a
butterfly f ws = uncurry (++) . unzip
               . zipWith f ws
               . uncurry zip . halve

fft :: (Syntax a, Num a)
    => Vector a -> Vector a -> Vector a
fft ws vs = forLoop (ilog2 len) (alloc vs) (copy stage)
  where
    len     = length vs
    stage s = chunk (len .>>. s) (butterfly dft2 (ixmap (.<<. s) ws))

ifft :: (Type a, RealFloat a, Numeric a, Fraction a)
     => Vector1 (Complex a) -> Vector1 (Complex a) -> Vector1 (Complex a)
ifft ws vs = map (/(i2n $ length vs)) $ fft (map conjugate ws) vs


---- { utils
ixmap :: (Syntax a)
      => (Data Index -> Data Index) -> Vector a -> Vector a
ixmap f = permute (const f)

halve :: (Syntax a) => Vector a -> (Vector a, Vector a)
halve v = splitAt (length v `div` 2) v

chunk :: (Syntax a, Syntax b)
      => Data Length -> (Vector a -> Vector b) -> Vector a -> Vector b
chunk c f v = flatten $ map f $ split v
  where
    l = length v
    r = l `div` c

    split w = indexed r $ \i ->
                indexed c $ \j -> w ! (c*i+j)

    flatten arr = indexed l ixf
      where
        ixf i = arr ! y ! x
          where
            y = i `div` c
            x = i `mod` c

---- } utils








