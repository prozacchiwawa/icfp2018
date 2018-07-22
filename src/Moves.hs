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
