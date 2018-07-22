module Types where

import Data.Bits
import Data.Word
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as B

data Axis = X | Y | Z deriving (Eq, Show)

data LLD = LLD Axis Int deriving (Eq, Show)
data SLD = SLD Axis Int deriving (Eq, Show)
data ND = ND Int Int Int deriving (Eq, Show)

data DVec = DVec Int Int Int deriving (Eq, Show, Ord)
        
data TraceCommand
    = Halt
    | Wait
    | Flip
    | SMove LLD
    | LMove SLD SLD
    | Fission Int ND
    | Fill ND
    | FusionP ND
    | FusionS ND deriving (Eq, Show)
      
makeSLD a i =
    case a of
      1 -> Just (SLD X (i - 5))
      2 -> Just (SLD Y (i - 5))
      3 -> Just (SLD Z (i - 5))
      _ -> Nothing

makeLLD a i =
    case a of
      1 -> Just (LLD X (i - 15))
      2 -> Just (LLD Y (i - 15))
      3 -> Just (LLD Z (i - 15))
      _ -> Nothing
      
decodeND datum =
    let z = mod datum 3 :: Int in
    let xy = div datum 3 :: Int in
    let y = mod xy 3 :: Int in
    let x = mod (div xy 3) 3 :: Int in
    ND (x - 1) (y - 1) (z - 1)

addVec :: DVec -> DVec -> DVec
addVec a@(DVec ax ay az) b@(DVec bx by bz) =
    DVec (ax+bx) (ay+by) (az+bz)

subVec :: DVec -> DVec -> DVec
subVec a@(DVec ax ay az) b@(DVec bx by bz) =
    DVec (ax-bx) (ay-by) (az-bz)
         
decodeSMove :: Int -> B.ByteString -> Int -> Maybe (Int, TraceCommand)
decodeSMove b f n =
    let c = B.index f (n+1) in
    let lld_a = shift b (-4) .&. 3 in
    let lld_i = c .&. 31 in do
      lld <- makeLLD lld_a (fromInteger (toInteger lld_i))
      pure (n+2, SMove lld)

intcoerce :: Word8 -> Int
intcoerce a =
    fromInteger (toInteger a)

nextPlane :: Axis -> Int -> DVec -> DVec
nextPlane axis n dv@(DVec x y z) =
    case axis of
      X -> DVec (x+n) y z
      Y -> DVec x (y+n) z
      Z -> DVec x y (z+n)

manhattanDistance :: DVec -> DVec -> Int
manhattanDistance (DVec sx sy sz) (DVec ex ey ez) =
    (abs (ex - sx)) + (abs (ey - sy)) + (abs (ez - sz))

