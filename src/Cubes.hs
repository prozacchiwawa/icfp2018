module Cubes where

import Data.Bits
import Data.Word
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
        isNonEmptyCube startDv (DVec (sx+1) sy 0) mt
    else if x >= sx + step then
        isNonEmptyCube startDv (DVec 0 (sy+1) 0) mt
    else if y >= sy + step then
        False
    else if MT.lookupTree dv mt then
        True
    else
        isNonEmptyCube startDv dv mt
    
doCubes_ acc f dv@(DVec x y z) mt =
    let n = MT.bound mt in
    let s = MT.cube mt in
    let u = DVec x y (z+s) in
    if z >= n then
        doCubes_ acc f (DVec (x+s) y 0) mt
    else if x >= n then
        doCubes_ acc f (DVec 0 (y+s) 0) mt
    else if y >= n then
        acc
    else if isNonEmptyCube dv dv mt then
        doCubes_ (f dv acc) f u mt
    else
        doCubes_ acc f dv mt

doCubes :: MT.ModelTree -> Set DVec
doCubes mt =
    doCubes_ Set.empty Set.insert (DVec 0 0 0) mt
    
