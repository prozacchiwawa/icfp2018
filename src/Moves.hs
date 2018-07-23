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
createPathThroughSpace_ :: (DVec -> Bool) -> ModelTree -> [(Int,DVec,[DVec])] -> Map DVec (Int, DVec, [DVec]) -> DVec -> Maybe [DVec]
createPathThroughSpace_ suitable mt queue resMap end =
    case queue of
      [] -> (trace ("failed to find path to " ++ (show end)) Nothing)
      (m,hd,l) : tl ->
          if hd == end then
              Just (trace ("path-to " ++ (show end) ++ " is " ++ (show l)) ([hd]++l))
          else
              let
                  moves =
                      List.filter
                              (\at -> (suitable at) && not (Map.member at resMap))
                              (MT.neighborMoves mt hd)

                  newElements =
                      List.map
                              (\a -> (manhattanDistance a end, a, [hd]++l))
                              moves
                              
                  newResMap =
                      List.foldl
                          (\m (ma,hd,l) ->
                               case Map.lookup hd resMap of
                                 Nothing ->
                                     Map.insert
                                        hd
                                        (ma,hd,l)
                                        m
                                 _ -> m
                          )
                          resMap
                          newElements
                          
                  sorted = List.sort (List.concat [ newElements, tl ])
              in
              createPathThroughSpace_ suitable mt (trace ("finding path to " ++ (show end) ++ " with " ++ (show (List.length sorted)) ++ " now " ++ (show (List.take 10 queue))) sorted) newResMap end

createPathThroughSpace :: ModelTree -> DVec -> DVec -> Maybe [DVec]
createPathThroughSpace mt start end =
    if lookupTree end mt || lookupTree start mt then
        Nothing
    else
        fmap
            List.reverse
            (createPathThroughSpace_
               (\at -> not (lookupTree at mt))
               mt
               [(manhattanDistance end start, start, [])]
               Map.empty
               end
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
                mt
                [(manhattanDistance end start, start, [])]
                Map.empty
                end
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
    
