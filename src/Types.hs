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
      
addVec :: DVec -> DVec -> DVec
addVec a@(DVec ax ay az) b@(DVec bx by bz) =
    DVec (ax+bx) (ay+by) (az+bz)

subVec :: DVec -> DVec -> DVec
subVec a@(DVec ax ay az) b@(DVec bx by bz) =
    DVec (ax-bx) (ay-by) (az-bz)
         
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

data ShapeID = ShapeID DVec deriving (Ord, Eq, Show)
data WShapeID = WShapeID DVec deriving (Ord, Eq, Show)
data CubeID = CubeID DVec deriving (Ord, Eq, Show)
             
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

optionDefault d v =
    case v of
      Just v -> v
      Nothing -> d
