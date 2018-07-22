module CommandQ where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
    
import Types
import qualified Moves as M
import qualified ModelTree as MT

simplePlotVoxels :: DVec -> [DVec] -> MT.ModelTree -> (DVec, [TraceCommand])
simplePlotVoxels at@(DVec x y z) voxels mt =
    case voxels of
      hd@(DVec hx hy hz) : tl ->
          if hz == z then
              let (next, cmds) = simplePlotVoxels at tl mt in
              ( next
              , List.concat
                    [ [Fill (ND 0 (-1) 0)]
                    , cmds
                    ]
              )
          else
              let (next, cmds) = simplePlotVoxels (DVec x y hz) tl mt in
              ( next
              , List.concat
                    [ [SMove (LLD Z (hz - z))]
                    , cmds
                    ]
              )
      [] -> (at, [])

simplePlotRow :: Int -> CubeID -> DVec -> [DVec] -> MT.ModelTree -> (DVec, [TraceCommand])
simplePlotRow n ci dv@(DVec x y z) voxels mt =
    let
        bound = MT.bound mt
        maxz z =
            if z + n >= bound then
                bound - 1
            else
                z + n - 1
        plots = List.sort voxels
    in
    
    simplePlotVoxels dv plots mt

simplePlotPlanes :: Int -> CubeID -> DVec -> Set DVec -> MT.ModelTree -> (DVec, [TraceCommand])
simplePlotPlanes n ci@(CubeID cvec@(DVec cx cy cz)) dv@(DVec x y z) voxels mt =
    let
        bound = MT.bound mt
        maxx x =
            if x + n >= bound then
                bound - 1
            else
                x + n - 1
        rows =
            List.filter
                (\row -> Set.size row > 0)
                (List.map
                         (\x -> Set.filter (\(DVec dx dy dz) -> dx == x+1 && dy == y) voxels)
                         [x .. (maxx (x - (mod x n)))]
                )
    in
    List.foldl
        (\(last, commands) row ->
            let
                firstOfRow =
                    case Set.toList row of
                      hd : tl -> hd
                      _ -> (DVec (x+1) y z)
                           
                move =
                    case fmap (pathCommands last) (M.createPathThroughSpace mt last firstOfRow) of
                      Just a -> a
                      Nothing -> []

                (l,c) = simplePlotRow n ci dv (Set.toList row) mt
            in
            (l, commands ++ move ++ c)
        )
        (dv, [])
        rows
    
{- Given a point list, make a trace list 
 - For now just emit a series of SMove
 -}
pathCommands at@(DVec ax ay az) ptlist =
    case ptlist of
      [] -> []
      hd@(DVec hx hy hz) : tl ->
          if hx /= ax then
              List.concat
                      [ [ SMove (LLD X (hx - ax)) ]
                      , pathCommands hd tl
                      ]
          else if hy /= ay then
              List.concat
                      [ [ SMove (LLD Y (hy - ay)) ]
                      , pathCommands hd tl
                      ]
          else if hz /= az then
              List.concat
                      [ [ SMove (LLD Z (hz - az)) ]
                      , pathCommands hd tl
                      ]
          else
              pathCommands at tl
    
{- Paint a single cube with energy on.
 - We assume that each y plane is completed before the next one is started.
 -}
backupPaintCube :: Int -> CubeID -> Map WShapeID (Set DVec) -> DVec -> MT.ModelTree -> MT.ModelTree -> (DVec,[TraceCommand])
backupPaintCube n ci@(CubeID cvec@(DVec cx cy cz)) shapes whereWas@(DVec x y z) cmt mt =
    let
        bound = MT.bound mt 
        yAbove y =
            if y == bound - 1 then
                (y - (mod y n)) + n
            else
                (y - (mod y n)) + (n - 1)
        (DVec fx fy fz) =
            fmap (\dv -> addVec dv cvec) (MT.scanForFirstGrounded cmt)
            |> optionDefault (DVec 0 (y+1) 0)
        aboveLocation = DVec x (yAbove y) z
        aboveOtherCubeLocation = DVec fx (yAbove fy) fz
        startPlottingLocation = DVec fx (fy+1) fz
        pathToAboveThisCube =
            fmap (pathCommands whereWas)
                (M.createPathThroughSpace mt whereWas aboveLocation)
            |> optionDefault []
        pathToTargetCube =
            fmap (pathCommands aboveLocation)
                (M.createPathThroughSpace mt aboveLocation aboveOtherCubeLocation)
            |> optionDefault []
        pathToStartOfPlotting =
            fmap (pathCommands aboveOtherCubeLocation)
                (M.createPathThroughSpace mt aboveOtherCubeLocation startPlottingLocation)
            |> optionDefault []
        allVoxels = Map.foldl Set.union Set.empty shapes
        (last, afterPlot) = simplePlotPlanes n ci startPlottingLocation allVoxels mt
    in
    ( last
    , List.concat
            [ pathToAboveThisCube
            , pathToTargetCube
            , pathToStartOfPlotting
            , afterPlot
            ]
    )
