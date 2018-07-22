module Cubes where

import Debug.Trace
import Data.Bits
import Data.Word
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString as B
import qualified System.Environment as SysEnv
import qualified Data.Octree as O

import Types
import qualified ModelTree as MT

isNonEmptyCube startDv size mt =
    let extracted = MT.extractCube startDv size mt in
    MT.foldZXY
        (\dv a ->
             if MT.lookupTree dv extracted then
                 True
             else
                 a
        )
        False
        extracted
    
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
    else if isNonEmptyCube dv s mt then
        doCubes_ (f dv acc) f u mt
    else
        doCubes_ acc f u mt

doCubes :: MT.ModelTree -> Set DVec
doCubes mt =
    doCubes_ Set.empty Set.insert (DVec 0 0 0) mt

neighborCubes :: DVec -> Int -> MT.ModelTree -> [DVec]
neighborCubes dv@(DVec x y z) s mt =
    let
        bounds = MT.bound mt
        toLeft =  DVec (x-s) y     z
        toRight = DVec (x+s) y     z
        below =   DVec x     (y-s) z
        above =   DVec x     (y+s) z
        ahead =   DVec x     y     (z-s)
        behind =  DVec x     y     (z+s)
        possible = [ toLeft, toRight, above, below, ahead, behind ]
    in
    List.filter
        (\at@(DVec x y z) ->
             x >= 0 && y >= 0 && z >= 0 &&
             x < bounds && y < bounds && z < bounds
        )
        possible

{- Find the grounded shapes -}
groundedShapes :: Map DVec (Set DVec) -> Map DVec Bool
groundedShapes shapes =
    Map.mapWithKey
       (\k v ->
            (List.filter
                (\dv@(DVec x y z) -> y == 0)
                (Set.toList v))
            /= []
       )
       shapes
