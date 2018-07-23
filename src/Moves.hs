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
createPathThroughSpace_ :: (DVec -> Bool) -> ModelTree -> [(Int,DVec)] -> [DVec] -> Set DVec -> DVec -> DVec -> Maybe [DVec]
createPathThroughSpace_ suitable mt queue res resSet start end =
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
                        (List.concat
                                 [ List.map
                                       (\a -> (manhattanDistance a end, a))
                                       moves
                                 , queue
                                 ]
                        )
        in
        tryOneAlternative sorted
    where
      tryOneAlternative alts =
          case alts of
            [] ->
                Nothing
            (m,hd) : tl ->
                let newResSet = Set.insert hd resSet in
                createPathThroughSpace_ suitable mt queue (hd : res) newResSet hd end

createPathThroughSpace :: ModelTree -> DVec -> DVec -> Maybe [DVec]
createPathThroughSpace mt start end =
    if lookupTree end mt || lookupTree start mt then
        Nothing
    else
        fmap
            List.reverse
            (createPathThroughSpace_
               (\at -> not (lookupTree at mt))
               mt [] [] (Set.insert start Set.empty) start end
            )

createPathThroughMatter :: ModelTree -> DVec -> DVec -> Maybe [DVec]
createPathThroughMatter mt start end =
    if not (lookupTree end mt) || not (lookupTree start mt) then
        Nothing
    else
        fmap
            List.reverse
            (createPathThroughSpace_
                (\at -> lookupTree at mt)
                mt [] [] (Set.insert start Set.empty) start end
            )

{- Given a point list, make a trace list 
 - For now just emit a series of SMove
 -}
pathCommands at@(DVec ax ay az) ptlist =
    case ptlist of
      [] -> (at, [])
      hd@(DVec hx hy hz) : tl ->
          let (last, path) = pathCommands hd tl in
          if hx /= ax then
              (last
              , List.concat
                    [ [ SMove (LLD X (hx - ax)) ]
                    , path
                    ]
              )
          else if hy /= ay then
              (last
              , List.concat
                    [ [ SMove (LLD Y (hy - ay)) ]
                    , path
                    ]
               )
          else if hz /= az then
              (last
              , List.concat
                    [ [ SMove (LLD Z (hz - az)) ]
                    , path
                    ]
               )
          else
              pathCommands at tl
    
