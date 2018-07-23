module ModelTree where

import Debug.Trace
import Data.Bits
import Data.Word
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as B
import qualified Linear.V3 as V3
import qualified Data.Octree as O

import Types

{-               
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
-}
makeTree :: Int -> B.ByteString -> ModelTree
makeTree n f =
    let actualSize = intcoerce (B.index f 0) in
    ModelTree
    { prev = Nothing
    , chunk = n
    , mscale = actualSize
    , oscale = actualSize
    , offset = 1
    , bits = f
    , filled = Set.empty
    }

emptyTree :: Int -> Int -> ModelTree
emptyTree n bound =
    ModelTree
    { prev = Nothing
    , chunk = n
    , mscale = 0
    , oscale = bound
    , offset = 0
    , bits = B.pack []
    , filled = Set.empty
    }

cube :: ModelTree -> Int
cube mt = chunk mt

scanForFirstGrounded :: DVec -> DVec -> ModelTree -> Maybe DVec
scanForFirstGrounded dv@(DVec x y z) e@(DVec ex ey ez) mt =
    if x >= ex then
        Nothing
    else if z >= ez then
        scanForFirstGrounded (DVec (x+1) y 0) e mt
    else
        if lookupTree dv mt then
            Just dv
        else
            scanForFirstGrounded (DVec x y (z+1)) e mt

neighborMoves_ :: DVec -> DVec -> ModelTree -> DVec -> [DVec]
neighborMoves_ (DVec nx ny nz) (DVec ax ay az) mt at@(DVec x y z) =
    let
        toLeft =  DVec (x-1) y     z
        toRight = DVec (x+1) y     z
        below =   DVec x     (y-1) z
        above =   DVec x     (y+1) z
        ahead =   DVec x     y     (z-1)
        behind =  DVec x     y     (z+1)
        possible = [ toLeft, toRight, above, below, ahead, behind ]
    in
    List.filter
        (\at@(DVec x y z) ->
             x >= nx && y >= ny && z >= nz &&
             x <  ax && y <  ay && z <  az
        )
        possible

{- Get the possible neighboring moves from here -}
neighborMoves mt at =
    let mtbounds = bound mt in
    neighborMoves_ (DVec 0 0 0) (DVec mtbounds mtbounds mtbounds) mt at
    
cubeNeigbhorMoves :: CubeID -> ModelTree -> DVec -> [DVec]
cubeNeigbhorMoves (CubeID cv@(DVec cx cy cz)) mt at =
    let
        c = cube mt
        u = DVec (cx+c) (cy+c) (cz+c)
    in
    neighborMoves_ cv u mt at
        
reverseBits :: Int -> Int -> Int -> Int
reverseBits i y x = 
    if i == 8 then
        y
    else
        reverseBits (i+1) (y * 2 + (mod x 2)) (div x 2)

makeBits :: Int -> Int -> [Word8] -> DVec -> DVec -> Int -> ModelTree -> B.ByteString
makeBits count bits res s@(DVec sx sy sz) a@(DVec x y z) newsize mt =
    if x >= sx + newsize then
        B.pack (List.reverse res)
    else if y >= sy + newsize then
        makeBits count bits res (DVec sx sy sz) (DVec (x+1) sy sz) newsize mt
    else if z >= sz + newsize then
        makeBits count bits res (DVec sx sy sz) (DVec x (y+1) sz) newsize mt
    else
        let
            newBits =
                if lookupTree (DVec x y z) mt then
                    (bits * 2) + 1
                else
                    bits * 2
                         
            newCount = count + 1
        in
        if newCount == 8 then
            makeBits 0 0 ([toEnum (reverseBits 0 0 bits)] ++ res) s (DVec x y (z+1)) newsize mt
        else
            makeBits newCount newBits res s (DVec x y (z+1)) newsize mt
        
addFilled :: DVec -> ModelTree -> ModelTree
addFilled dv mt =
    let newFilled = Set.insert dv (filled mt) in
    mt { filled = newFilled }

addFilledSet :: Set DVec -> ModelTree -> ModelTree
addFilledSet ds mt =
    let newFilled = Set.union ds (filled mt) in
    mt { filled = newFilled }
       
foldZXY_ u@(DVec ux uy uz) s@(DVec sx sy sz) dv@(DVec x y z) f a mt =
    if z >= uz then
        foldZXY_ u s (DVec (x+1) y sz) f a mt
    else if x >= ux then
        foldZXY_ u s (DVec sx (y+1) sz) f a mt
    else if y >= uy then
        a
    else
        foldZXY_ u s (DVec x y (z+1)) f (f dv a) mt

foldZXY f a mt =
    let b = bound mt in
    foldZXY_ (DVec b b b) (DVec 0 0 0) (DVec 0 0 0) f a mt
           
cubeFoldZXY (CubeID sv@(DVec cx cy cz)) f a mt =
    let c = cube mt in
    foldZXY_ (DVec (cx + c) (cy + c) (cz + c)) sv sv f a mt
