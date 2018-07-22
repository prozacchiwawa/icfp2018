module Moves where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Types
import qualified ModelTree as MT

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
                        (\at -> not (MT.lookupTree at mt) && not (Set.member at resSet))
                        (MT.neighborMoves mt start)
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
