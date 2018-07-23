module Types where

import Data.Bits
import Data.Word
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString as B
import qualified Data.Octree as O

data Axis = X | Y | Z deriving (Ord, Eq, Show)

data LLD = LLD Axis Int deriving (Ord, Eq, Show)
data SLD = SLD Axis Int deriving (Ord, Eq, Show)
data ND = ND Int Int Int deriving (Ord, Eq, Show)

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
    | FusionS ND deriving (Ord, Eq, Show)
      
data ConnectomeTree = ConnectomeTree WShapeID [ConnectomeTree] deriving (Ord, Show, Eq)
data Connectome = Connectome ([Map WShapeID [(WShapeID,WShapeID)]]) deriving (Ord, Show, Eq)
data WorldShapes = WorldShapes (Map CubeID (Map WShapeID (Set DVec)))

{- A hopefully more efficient representation of the model space that will be convenient for
 - making copies of and updating.  We'll try to balance the number of nodes in the tree with
 - the number of bits representing spaces to get a good size speed compromise.
 -
 - mscale determines the size of a leaf ByteString if it is a copy.
 - oscale determines the size of the leaves in this tree.
 -}

data ModelTree
    = ModelTree
      { prev :: Maybe ModelTree
      , chunk :: Int
      , mscale :: Int
      , oscale :: Int
      , offset :: Int
      , bits :: B.ByteString
      , filled :: Set DVec
      } deriving (Ord, Eq)

bound :: ModelTree -> Int
bound mt = oscale mt

bitByteOfCoord offset scale x y z =
    let bitNumber = x * scale * scale + y * scale + z in
    let byteOffset = offset + (quot bitNumber 8) in
    let bitOffset = mod bitNumber 8 in
    (byteOffset, bitOffset)
           
lookupInNode offset scale dv@(DVec x y z) bits =
    let
        (byteOffset, bitOffset) = bitByteOfCoord offset scale x y z
        word = B.index bits byteOffset
        bitValue = (shift word (bitOffset * (-1))) .&. 1
    in
    bitValue /= 0

lookupTree :: DVec -> ModelTree -> Bool
lookupTree dv@(DVec ox oy oz) mt =
    let
        os = mscale mt
    in
    if
        (Set.member dv (filled mt)) ||
           (ox >= 0 && oy >= 0 && oz >= 0 &&
               ox < os && oy < os && oz < os &&
               lookupInNode (offset mt) os dv (bits mt))
    then
        True
    else
        prev mt
        |> fmap (lookupTree dv)
        |> optionDefault False

showSlice :: DVec -> ModelTree -> String
showSlice dv@(DVec x y z) mt =
    if x >= bound mt then
        "|\n" ++ (showSlice (DVec 0 y (z+1)) mt)
    else if z >= bound mt then
        ""
    else if lookupTree dv mt then
        "@@" ++ (showSlice (DVec (x+1) y z) mt)
    else
        "  " ++ (showSlice (DVec (x+1) y z) mt)
            
showSlices_ :: Int -> ModelTree -> String
showSlices_ y mt =
    if y >= bound mt then
        ""
    else
        ("--- " ++ (show y) ++ " ---\n" ++
                    (showSlice (DVec 0 y 0) mt) ++ "\n" ++ (showSlices_ (y+1) mt)
        )
                       
showSlices :: ModelTree -> String
showSlices mt =
    "filled: " ++ (show (filled mt)) ++ "\n" ++ (showSlices_ 0 mt)

instance Show ModelTree where
    show = showSlices

data ExtractedCubes = ExtractedCubes (Map CubeID ModelTree)
                    
data PrintHead =
    PrintHead
    { at :: DVec
    , volatile :: Set DVec
    } deriving (Ord, Eq, Show)
    
data Machine =
    Machine
    { ph :: PrintHead
    , tree :: ModelTree
    , heater :: Bool
    , commands :: [TraceCommand]
    , outcome :: Maybe String
    } deriving (Ord, Eq, Show)

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

data WShapeID = WShapeID DVec deriving (Ord, Eq, Show)
data CubeID = CubeID DVec deriving (Ord, Eq, Show)
             
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

optionThen f v =
    case v of
      Just v -> f v
      Nothing -> Nothing
    
optionDefault d v =
    case v of
      Just v -> v
      Nothing -> d

cubeIDFromWShapeID :: WShapeID -> CubeID
cubeIDFromWShapeID (WShapeID (DVec wx wy wz)) =
    let
        cx = wx - (mod wx 8)
        cy = wy - (mod wy 8)
        cz = wz - (mod wz 8)
    in
    CubeID (DVec cx cy cz)
