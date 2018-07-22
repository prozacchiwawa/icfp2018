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

isNonEmptyCube (CubeID startDv) size mt =
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
    
doCubes_ acc f ci@(CubeID (DVec x y z)) mt =
    let n = MT.bound mt in
    let s = MT.cube mt in
    let u = CubeID (DVec x y (z+s)) in
    if (z+s) >= n then
        doCubes_ acc f (CubeID (DVec (x+s) y 0)) mt
    else if x >= n then
        doCubes_ acc f (CubeID (DVec 0 (y+s) 0)) mt
    else if y >= n then
        acc
    else if isNonEmptyCube ci s mt then
        doCubes_ (f ci acc) f u mt
    else
        doCubes_ acc f u mt

doCubes :: MT.ModelTree -> Set CubeID
doCubes mt =
    doCubes_ Set.empty Set.insert (CubeID (DVec 0 0 0)) mt

neighborCubes :: CubeID -> Int -> MT.ModelTree -> [CubeID]
neighborCubes (CubeID (DVec x y z)) s mt =
    let
        bounds = MT.bound mt
        toLeft =  DVec (x-s) y     z
        toRight = DVec (x+s) y     z
        below =   DVec x     (y-s) z
        above =   DVec x     (y+s) z
        ahead =   DVec x     y     (z-s)
        behind =  DVec x     y     (z+s)
        possible =
            [ CubeID toLeft
            , CubeID toRight
            , CubeID above
            , CubeID below
            , CubeID ahead
            , CubeID behind ]
    in
    List.filter
        (\(CubeID at@(DVec x y z)) ->
             x >= 0 && y >= 0 && z >= 0 &&
             x < bounds && y < bounds && z < bounds
        )
        possible

getWorldShapes :: Map CubeID (Map ShapeID (Set DVec)) -> Map CubeID (Map WShapeID (Set DVec))
getWorldShapes shapes =
    Map.mapWithKey
           (\k v ->
                Cubes.mapShapesToWorldSpace k v
           )
           shapes
        
{- Map shapes to world space -}
mapShapesToWorldSpace :: CubeID -> Map ShapeID (Set DVec) -> Map WShapeID (Set DVec)
mapShapesToWorldSpace (CubeID dv) shapes =
    Map.fromList
       (List.map (\((ShapeID k),v) -> (WShapeID (addVec k dv), Set.map (addVec dv) v))
                (Map.toList shapes)
       )
       
{- Find the grounded shapes -}
groundedShapes :: Map WShapeID (Set DVec) -> Map WShapeID Bool
groundedShapes shapes =
    Map.mapWithKey
       (\k v ->
            (List.filter
                (\dv@(DVec x y z) -> y == 0)
                (Set.toList v))
            /= []
       )
       shapes

shellShape :: Int -> Map WShapeID (Set DVec) -> Map WShapeID (Set DVec)
shellShape n shapes =
    Map.mapWithKey
       (\k v ->
            Set.fromList
               (List.filter
                    (\dv@(DVec x y z) ->
                         let
                             nn = n - 1
                             xmn = mod x n 
                             ymn = mod y n
                             zmn = mod z n
                      in
                      xmn == 0 || ymn == 0 || zmn == 0 || xmn == nn || ymn == nn || zmn == nn
                    )
                    (Set.toList v)
               )
       )
       shapes

{- Find the neighbors of the shape shell that lie outside the same cube. -}
externalNeighbors :: Int -> Map WShapeID (Set DVec) -> MT.ModelTree -> Map WShapeID (Set DVec)
externalNeighbors n shapes mt =
    Map.mapWithKey
       (\(WShapeID k@(DVec x y z)) v ->
            let
                tcx = x - (mod x 8)
                tcy = y - (mod y 8)
                tcz = z - (mod z 8)
                outsideX = Set.fromList [tcx - 1, tcx + n]
                outsideY = Set.fromList [tcy - 1, tcy + n]
                outsideZ = Set.fromList [tcz - 1, tcz + n]
                allNeighbors =
                    List.foldl
                        (\s n ->
                             let
                                 neighbors = MT.neighborMoves mt n
                                 outsideNeighbors =
                                     List.filter
                                         (\n@(DVec x y z) ->
                                              Set.member x outsideX ||
                                                 Set.member y outsideY ||
                                                 Set.member z outsideZ
                                         )
                                         neighbors
                             in
                             Set.union (Set.fromList outsideNeighbors) s
                        )
                        Set.empty
                        (Set.toList v)
            in
            allNeighbors
       )
       shapes
        
{- Adjacent shapes 
 - Given the position and shapes of cube 1 and position and shapes of cube2,
 - output a list of connected shapes from c1 to c2.
 -}
adjacentShapes :: Int -> Map WShapeID (Set DVec) -> Map WShapeID (Set DVec) -> MT.ModelTree -> Map WShapeID [(WShapeID,WShapeID)]
adjacentShapes n c1 c2 mt =
    let
        {- shellsC1 is a shape-identifier-indexed map of potentially overlapping points for
         - other cubes.
         - If one of these appears in a shape in another cube then the shapes are connected.
         -}
        shellsC1 = externalNeighbors n (shellShape n c1) mt
        c2List = Map.toList c2

        c2ListMatchingShellsC1 =
            Map.mapWithKey
               (\k v ->
                    let
                        matchingListFromC2 =
                            List.map
                                (\(k,i) ->  (k,Set.intersection v i))
                                c2List

                        onlyMatchingC2 =
                            List.filter (\(k,i) -> Set.size i > 0) matchingListFromC2
                    in
                    List.map (\(c2k,i) -> (k,c2k)) onlyMatchingC2
               )
               shellsC1
    in
    Map.filter (\v -> List.length v > 0) c2ListMatchingShellsC1

groundedShapeSet :: Map CubeID (Map WShapeID (Set DVec)) -> Set WShapeID
groundedShapeSet wshapes =                                                       
    let
        groundedShapesInCubes =
            Map.mapWithKey
                   (\k v ->
                        Cubes.groundedShapes v
                   )
                   wshapes

        groundedShapes = Map.elems groundedShapesInCubes
        grounded =
            List.map
                    (\(k,v) -> k)
                    (List.filter (\(k,v) -> v)
                             (List.concat (List.map Map.toList groundedShapes))
                    )
    in
    Set.fromList grounded

getConnectome :: Map CubeID (Map WShapeID (Set DVec)) -> MT.ModelTree -> [Map WShapeID [(WShapeID,WShapeID)]]
getConnectome wshapes mt =
    let
        connectomeRaw =
            List.map
                    (\(k1,v1) ->
                         List.map
                                 (\(k2,v2) ->
                                      if k1 < k2 then
                                          [adjacentShapes 8 v1 v2 mt]
                                      else
                                          []
                                 )
                                 (Map.toList wshapes)
                    )
                    (Map.toList wshapes)
    in
    List.filter (\m -> (Map.size m) > 0) (List.concat (List.concat connectomeRaw))
