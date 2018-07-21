module Cubes where

import Data.Bits
import Data.Word
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString as B
import qualified System.Environment as SysEnv
import qualified Data.Octree as O

import Types
import qualified ModelTree as MT

isNonEmptyCube startDv@(DVec sx sy sz) dv@(DVec x y z) mt =
    let step = MT.cube mt in
    if z >= sz + step then
        isNonEmptyCube startDv (DVec (x+1) y 0) mt
    else if x >= sx + step then
        isNonEmptyCube startDv (DVec 0 (y+1) 0) mt
    else if y >= sy + step then
        False
    else if MT.lookupTree dv mt then
        True
    else
        isNonEmptyCube startDv (DVec x y (z+1)) mt
    
doCubes_ acc f dv@(DVec x y z) mt =
    let n = MT.bound mt in
    let s = MT.cube mt in
    let u = DVec x y (z+s) in
    if (z+s) >= n then
        doCubes_ acc f (DVec (x+s) y 0) mt
    else if x >= n then
        doCubes_ acc f (DVec 0 (y+s) 0) mt
    else if y >= n then
        acc
    else if isNonEmptyCube dv dv mt then
        doCubes_ (f dv acc) f u mt
    else
        doCubes_ acc f u mt

doCubes :: MT.ModelTree -> Set DVec
doCubes mt =
    doCubes_ Set.empty Set.insert (DVec 0 0 0) mt

{- Get the possible neighboring moves from here -}
neighborMoves :: MT.ModelTree -> [DVec] -> DVec -> [DVec]
neighborMoves mt res at@(DVec x y z) =
    let
        bounds = MT.bound mt
        toLeft = DVec (x-1) y z
        toRight = DVec (x+1) y z
        above = DVec x (y+1) z
        below = DVec x (y-1) z
        ahead = DVec x y (z-1)
        behind = DVec x y (z+1)
        possible = [ toLeft, toRight, above, below, ahead, behind ]
    in
    List.filter
        (\at@(DVec x y z) ->
             x >= 0 && y >= 0 && z >= 0 &&
             x < bounds && y < bounds && z < bounds &&
             (not (MT.lookupTree at mt))
        )
        possible

manhattanDistance :: DVec -> DVec -> Int
manhattanDistance (DVec sx sy sz) (DVec ex ey ez) =
    (abs (ex - sx)) + (abs (ey - sy)) + (abs (ez - sz))

{- Path through space given tree object that specifies the taken elements in the space.
 -}
createPathThroughSpace_ :: MT.ModelTree -> [DVec] -> Set DVec -> DVec -> DVec -> Maybe [DVec]
createPathThroughSpace_ mt res resSet start end =
    if start == end then
        Just res
    else
        let
            moves =
                List.filter
                        (\at -> not (Set.member at resSet))
                        (neighborMoves mt res start)
            sorted =
                List.sort
                    (List.map
                             (\a -> (manhattanDistance a end, a))
                             moves
                    )
        in
        runThroughAlternatives (List.map (\(x,y) -> y) sorted)
    where
      runThroughAlternatives alts =
          case alts of
            [] ->
              Nothing
            hd : tl ->
                let newResSet = Set.insert hd resSet in
                case createPathThroughSpace_ mt (hd : res) newResSet hd end of
                  Just path ->
                      Just path
                  Nothing ->
                      runThroughAlternatives tl

createPathThroughSpace :: MT.ModelTree -> DVec -> DVec -> Maybe [DVec]
createPathThroughSpace mt start end =
    if MT.lookupTree end mt || MT.lookupTree start mt then
        Nothing
    else
        fmap
            List.reverse
            (createPathThroughSpace_ mt [] (Set.insert start Set.empty) start end)
                             
{- Maneuver to the upper left forward corner of the 6x6 cube at the center of the 8x8 cube.
 - Draw it by scanning over it from bottom to top, drawing below.
 - Be at the upper right corner when finished.
 -
 - We'll start in 
 -}
doPathThroughCubes_ :: MT.ModelTree -> [DVec] -> [DVec] -> DVec -> DVec -> Maybe [DVec]
doPathThroughCubes_ mt cubes toFill ptn lastLoc =
    Nothing
