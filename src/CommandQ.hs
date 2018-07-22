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
    case trace ("at " ++ (show at) ++ " voxels " ++ (show voxels)) voxels of
      hd@(DVec hx hy hz) : tl ->
          if hx == x && hy == y - 1 && hz == z then
              let (next, cmds) = simplePlotVoxels at tl mt in
              ( next
              , List.concat
                    [ [Fill (ND 0 (-1) 0)]
                    , cmds
                    ]
              )
          else
              let
                  (afterMove, moves) = pathCommands at [(DVec hx (hy+1) hz)]
                  (next, cmds) = simplePlotVoxels afterMove voxels mt
              in
              ( next
              , List.concat
                    [ moves
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

simplePlotPlane :: Int -> CubeID -> DVec -> Set DVec -> MT.ModelTree -> (DVec, [TraceCommand])
simplePlotPlane n ci@(CubeID cvec@(DVec cx cy cz)) dv@(DVec x y z) voxels mt =
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
                         (\x -> Set.filter (\(DVec dx dy dz) -> dx == x) voxels)
                         [(x+cx) .. (maxx ((x+cx) - (mod x n)))]
                )
    in
    List.foldl
        (\(last, commands) row ->
            let
                (DVec fx fy fz) =
                    case Set.toList row of
                      hd : tl -> hd
                      _ -> (DVec (x+1) y z)
                           
                (toward, move) =
                    case fmap (pathCommands last) (M.createPathThroughSpace mt last (DVec fx (fy+1) fz)) of
                      Just a -> a
                      Nothing -> (last, [])

                (l,c) = simplePlotRow n ci toward (trace ("simplePlotRow " ++ (show row)) (Set.toList row)) mt
            in
            (l, commands ++ move ++ c)
        )
        (dv, [])
        rows    
      
simplePlotPlanes :: Int -> CubeID -> DVec -> Set DVec -> MT.ModelTree -> (DVec, [TraceCommand])
simplePlotPlanes n ci@(CubeID cvec@(DVec cx cy cz)) dv@(DVec x y z) voxels mt =
    let
        bound = MT.bound mt
        maxy y =
            if y + n >= bound then
                bound - 1
            else
                y + n - 1

        planes =
            List.filter
                    (\plane -> Set.size plane > 0)
                    (List.map
                             (\y -> Set.filter (\(DVec dx dy dz) -> dy == y) voxels)
                             [0 .. (maxy (y - (mod y n)))]
                    )
    in
    List.foldl
        (\(last,commands) plane ->
            let
                firstOfPlane = DVec x (y+1) z
                (toward, move) =
                    case fmap (pathCommands last) (M.createPathThroughSpace mt last firstOfPlane) of
                      Just a -> a
                      Nothing -> (last, [])

                (l,c) = simplePlotPlane n ci toward (trace ("plane " ++ (show plane)) plane) mt
            in
            (l, commands ++ move ++ c)
        )
        (dv, [])
        planes
    
{- Given a point list, make a trace list 
 - For now just emit a series of SMove
 -}
pathCommands at@(DVec ax ay az) ptlist =
    case (trace ("at " ++ (show at) ++ " pathCommands " ++ (show ptlist)) ptlist) of
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
    
{- Paint a single cube with energy on.
 - We assume that each y plane is completed before the next one is started.
 -}
backupPaintCube :: Int -> CubeID -> Map WShapeID (Set DVec) -> DVec -> MT.ModelTree -> MT.ModelTree -> (DVec,[TraceCommand])
backupPaintCube n ci@(CubeID cvec@(DVec cx cy cz)) shapes whereWas@(DVec x y z) cmt mt =
    let
        bound = MT.bound mt 
        yAbove y =
            let ym = y - (mod y n) + n in
            if ym >= bound then
                bound - 1
            else
                ym
                      
        slice = MT.extractCube cvec n mt
        (DVec fx fy fz) =
            fmap (\dv -> addVec dv cvec) (MT.scanForFirstGrounded slice)
            |> optionDefault (DVec cx (cy+1) cz)

        aboveLocation = DVec x (yAbove y) z
        aboveOtherCubeLocation = DVec fx (yAbove fy) fz
        startPlottingLocation = DVec fx fy fz
                                
        (ptaLast, pathToAboveThisCube) =
            fmap (pathCommands whereWas)
                (M.createPathThroughSpace mt whereWas (trace ("aboveLocation " ++ (show aboveLocation)) aboveLocation))
            |> optionDefault (whereWas, [])
               
        (pttLast, pathToTargetCube) =
            fmap (pathCommands aboveLocation)
                (M.createPathThroughSpace mt ptaLast (trace ("aboveOtherCubeLocation " ++ (show aboveOtherCubeLocation)) aboveOtherCubeLocation))
            |> optionDefault (ptaLast, [])
               
        (ptsLast, pathToStartOfPlotting) =
            fmap (pathCommands aboveOtherCubeLocation)
                (M.createPathThroughSpace mt pttLast (trace ("startPlottingLocation " ++ (show startPlottingLocation)) startPlottingLocation))
            |> optionDefault (pttLast, [])
               
        allVoxels = Map.foldl Set.union Set.empty shapes
                    
        (last, afterPlot) =
            simplePlotPlanes
                n ci ptsLast (trace ("allVoxels " ++ (show allVoxels)) allVoxels) cmt
                            
    in
    ( last
    , List.concat
            [ pathToAboveThisCube
            , pathToTargetCube
            , pathToStartOfPlotting
            , afterPlot
            ]
    )
