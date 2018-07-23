module Region where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Types
import qualified ModelTree as MT

{- Given a modeltree, make a map where each full voxel DV contains itself in the map. -}
initForShapeFinding :: CubeID -> ModelTree -> Map DVec WShapeID
initForShapeFinding ci mt =
    MT.cubeFoldZXY
      ci
      (\dv a ->
           if lookupTree dv mt then
               Map.insert dv (WShapeID dv) a
           else
               a
      )
      Map.empty
      mt

getShapesInCube_ :: CubeID -> Map DVec WShapeID -> ModelTree -> Map DVec WShapeID
getShapesInCube_ cid m mt =
    MT.cubeFoldZXY
      cid
      (\dv m ->
       if lookupTree dv mt then
           let 
               neighborhood =
                   List.filter
                           (\n -> lookupTree n mt)
                           ([dv] ++ (MT.cubeNeigbhorMoves cid mt dv))

               neighborhoodBelongsTo =
                   List.map
                           (\a ->
                                case Map.lookup a m of
                                  Just a -> a
                                  Nothing -> (WShapeID a)
                           )
                       neighborhood

               bestNeighbor =
                   case neighborhoodBelongsTo of
                     [] -> (WShapeID dv)
                     _ -> minimum neighborhoodBelongsTo
           in
           List.foldl
              (\m e -> Map.insert e bestNeighbor m)
              m
              neighborhood
       else
           m
      )
      m
      mt

getShapesInCube :: CubeID -> ModelTree -> Map DVec WShapeID
getShapesInCube cid mt = do
    let sinit = initForShapeFinding cid mt
    untilSame (==) (\m -> getShapesInCube_ cid m mt) sinit
    where
      untilSame sameValues makeNewMap sinit =
          let newMap = makeNewMap sinit in
          if sameValues newMap sinit then
              sinit
          else
              untilSame sameValues makeNewMap newMap

getRegionLabels :: Map DVec WShapeID -> Set WShapeID
getRegionLabels m =
    Set.fromList (List.map (\(x,y) -> y) (Map.toList m))


{- Turn the recovered regions into sets of DVec -}
shapeFromRegions shapes mt =
    let allAssocs = Map.toList shapes in
    List.foldl
        (\m (k,v) ->
             case Map.lookup v m of
               Just s -> Map.insert v (Set.insert k s) m
               Nothing -> Map.insert v (Set.insert k Set.empty) m
        )
        Map.empty
        allAssocs
