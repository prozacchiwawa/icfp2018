module Region where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Types
import qualified ModelTree as MT

{- Given a modeltree, make a map where each full voxel DV contains itself in the map. -}
initForShapeFinding :: MT.ModelTree -> Map DVec DVec
initForShapeFinding mt =
    MT.foldZXY
      (\dv a ->
           if MT.lookupTree dv mt then
               Map.insert dv dv a
           else
               a
      )
      Map.empty
      mt

getShapesInCube_ :: Map DVec DVec -> MT.ModelTree -> Map DVec DVec
getShapesInCube_ m mt =
    MT.foldZXY
      (\dv m ->
       if MT.lookupTree dv mt then
           let 
               neighborhood =
                   List.filter
                           (\n -> MT.lookupTree n mt)
                           ([dv] ++ (MT.neighborMoves mt dv))

               neighborhoodBelongsTo =
                   List.map
                           (\a ->
                                case Map.lookup a m of
                                  Just a -> a
                                  Nothing -> a
                           )
                       neighborhood

               bestNeighbor =
                   case neighborhoodBelongsTo of
                     [] -> dv
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

getShapesInCube :: MT.ModelTree -> Map DVec DVec
getShapesInCube mt = do
    let sinit = initForShapeFinding mt
    untilSame (==) (\m -> getShapesInCube_ m mt) sinit
    where
      untilSame sameValues makeNewMap sinit =
          let newMap = makeNewMap sinit in
          if sameValues newMap sinit then
              sinit
          else
              untilSame sameValues makeNewMap newMap

getRegionLabels :: Map DVec DVec -> Set DVec
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
