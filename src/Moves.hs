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
createPathThroughSpace_ :: (DVec -> Bool) -> MT.ModelTree -> [DVec] -> Set DVec -> DVec -> DVec -> Maybe [DVec]
createPathThroughSpace_ suitable mt res resSet start end =
    if start == end then
        Just res
    else
        let
            moves =
                List.filter
                        (\at -> (suitable at) && not (Set.member at resSet))
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
                case createPathThroughSpace_ suitable mt (hd : res) newResSet hd end of
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
            (createPathThroughSpace_
               (\at -> not (MT.lookupTree at mt))
               mt [] (Set.insert start Set.empty) start end
            )

createPathThroughMatter :: MT.ModelTree -> DVec -> DVec -> Maybe [DVec]
createPathThroughMatter mt start end =
    if MT.lookupTree end mt || MT.lookupTree start mt then
        Nothing
    else
        fmap
            List.reverse
            (createPathThroughSpace_
                (\at -> MT.lookupTree at mt)
                mt [] (Set.insert start Set.empty) start end
            )
