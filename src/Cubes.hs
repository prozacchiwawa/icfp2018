module Cubes where

import Debug.Trace
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

