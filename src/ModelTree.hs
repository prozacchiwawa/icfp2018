module ModelTree where

import Data.Bits
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as B
import qualified Linear.V3 as V3
import qualified Data.Octree as O

import Types
    
{- A hopefully more efficient representation of the model space that will be convenient for
 - making copies of and updating.  We'll try to balance the number of nodes in the tree with
 - the number of bits representing spaces to get a good size speed compromise.
 -
 - mscale determines the size of a leaf ByteString if it is a copy.
 - oscale determines the size of the leaves in this tree.
 -}
data ModelTree = ModelTree
    { prev :: Maybe ModelTree
    , mscale :: Int
    , oscale :: Int
    , offset :: Int
    , tree :: O.Octree B.ByteString
    }

data MapLayer = MapLayer
    { next :: Maybe MapLayer
    , dim :: Int
    , layer :: B.ByteString
    }
               
-- We will need to shorthand items in process in leaves in order to be able to compute with
-- enough space to work.
--
-- I will need a space filling algorithm I can rely on to store in a leaf when I know it will
-- be filled.
-- 
-- precompute efficient paths through at most 32768 patterns present in each figure and disk
-- store.
--
-- Know the indices of these but not how they work (along with entry, exit coords).
--
-- Backup plan for if the place we want our emergency burrow to be can't be used (because it
-- intersects a 1 or 2 thick line).
-- They might troll me with the backup line.  We need to actually plot the backup line
-- properly.  Requirements:
--  - it provides turnouts for passing nanites
--  - it connects to adjacent squares' backup lines
--
-- Times when the turnout is tricky:
--
-- An entire block that connects to grnd coords only on one side.
-- It has this kind of pattern throughout:
-- .-.-
-- We can't make a single line backup line through this because there is nowhere we can be
-- guaranteed not to change the connectedness.
--
-- That is a core property of the backup line.  It never changes the grndness of a present
-- voxel.  It erases voxels that are within fill range of the backup line only.
--
-- Disk activity?
-- If needed, we can turn on the power and do space filling.
-- Secondly, we can try to see if any of 4 directional space filling strategies lets us do
-- the figure with low power.
-- Separately, we can figure out whether the figure can be traversed and whether building a
-- backup line would work.
--
-- Current plan:
--
-- Choose an interval and see how many backup lines i can draw near those intervals in
-- X Y and Z.
-- Make a program that draws the figure with the power on, first without filling the backup
-- lines, then filling the backup lines with the power off.
--
-- The decide how to proceed.
--

makeTree :: Int -> B.ByteString -> ModelTree
makeTree n f =
    let actualSize = intcoerce (B.index f 0) in
    ModelTree
    { prev = Nothing
    , mscale = n
    , oscale = actualSize
    , offset = 1
    , tree = O.fromList [ (V3.V3 0.0 0.0 0.0, f) ]
    }

emptyTree :: Int -> Int -> ModelTree
emptyTree n bound =
    ModelTree
    { prev = Nothing
    , mscale = n
    , oscale = bound
    , offset = 0
    , tree = O.fromList []
    }

cube :: ModelTree -> Int
cube mt = mscale mt

bound :: ModelTree -> Int
bound mt = oscale mt

lookupInNode offset scale x y z bits =
    let bitNumber = x * scale * scale + y * scale + z in
    let byteOffset = div (offset + bitNumber) 8 in
    let bitOffset = mod bitNumber 8 in
    let word = B.index bits byteOffset in
    let bitValue = (shift word (bitOffset * (-1))) .&. 1 in
    bitValue /= 0

scanForFirstGrounded_ :: ModelTree -> DVec -> Maybe DVec
scanForFirstGrounded_ mt dv@(DVec x y z) =
    if x >= bound mt then
        Nothing
    else if z >= bound mt then
        scanForFirstGrounded_ mt (DVec (x+1) y 0)
    else
        if lookupTree dv mt then
            Just dv
        else
            scanForFirstGrounded_ mt (DVec x y (z+1))

scanForFirstGrounded :: ModelTree -> Maybe DVec
scanForFirstGrounded mt = scanForFirstGrounded_ mt (DVec 0 0 0)

lookupTree :: DVec -> ModelTree -> Bool
lookupTree (DVec x y z) mt =
    let
        scale = mscale mt

        xNode = div x scale
        yNode = div y scale
        zNode = div z scale
        mbits = 
            O.lookup
                 (tree mt)
                 (V3.V3 (fromIntegral xNode) (fromIntegral yNode) (fromIntegral zNode))
    in
    case mbits of
      Just (_,bits) ->
          lookupInNode (offset mt) scale (mod x scale) (mod y scale) (mod z scale) bits
      Nothing ->
          case (prev mt) of
            Just p -> lookupTree (DVec x y z) p
            Nothing -> False

