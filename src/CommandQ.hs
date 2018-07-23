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
import qualified MachineState as MS
    
simplePlotVoxels :: [DVec] -> ModelTree -> Machine -> Machine
simplePlotVoxels voxels targetModel machine =
    let at@(DVec x y z) = MS.getAt (MS.getPrintHead machine) in
    case trace ("at " ++ (show at) ++ " voxels " ++ (show voxels)) voxels of
      hd@(DVec hx hy hz) : tl ->
          if hx == x && hy == y - 1 && hz == z then
              machine
              |> MS.executeCommands [Fill (ND 0 (-1) 0)]
              |> simplePlotVoxels tl targetModel
          else
              machine
              |> MS.navigateTo (DVec hx (hy+1) hz)
              |> simplePlotVoxels voxels targetModel
      [] -> machine

simplePlotRow :: CubeID -> [DVec] -> ModelTree -> Machine -> Machine
simplePlotRow ci voxels targetModel machine =
    let
        n = MT.cube targetModel
        (DVec x y z) = MS.getAt (MS.getPrintHead machine)
        mtbound = bound targetModel
        maxz z =
            if z + n >= mtbound then
                mtbound - 1
            else
                z + n - 1
        plots = List.sort voxels
    in
    simplePlotVoxels plots targetModel machine

simplePlotPlane :: CubeID -> Set DVec -> ModelTree -> Machine -> Machine
simplePlotPlane ci@(CubeID cvec@(DVec cx cy cz)) voxels targetModel machine =
    let
        n = MT.cube targetModel
        mtbound = bound targetModel
        (DVec x y z) = MS.getAt (MS.getPrintHead machine)
        maxx x =
            if x + n >= mtbound then
                mtbound - 1
            else
                x + n - 1

        rows =
            List.filter
                (\row -> Set.size row > 0)
                (List.map
                         (\x -> Set.filter (\(DVec dx dy dz) -> dx == x) voxels)
                         [cx .. (maxx (x - (mod x n)))]
                )
    in
    List.foldl
        (\machine row ->
            let
                (DVec fx fy fz) =
                    case row of
                      hd : tl -> hd
                      _ -> (DVec (x+1) y z)
            in
            machine
            |> MS.navigateTo (DVec fx (fy+1) fz)
            |> simplePlotRow ci row targetModel
        )
        machine
        (List.map Set.toList rows)
      
simplePlotPlanes :: CubeID -> Set DVec -> ModelTree -> Machine -> Machine
simplePlotPlanes ci@(CubeID cvec@(DVec cx cy cz)) voxels targetModel machine =
    let
        n = MT.cube targetModel
        mtbound = bound targetModel
        dv@(DVec x y z) = MS.getAt (MS.getPrintHead machine)
        maxy y =
            let yn = y - (mod y n) in
            if yn + n >= mtbound then
                mtbound - 2
            else
                yn + n

        planes =
            List.filter
                    (\plane -> Set.size plane > 0)
                    (List.map
                             (\y -> Set.filter (\(DVec dx dy dz) -> dy == y) voxels)
                             [cy .. (trace ("simplePlotPlanes " ++ (show ci) ++ " height " ++ (show (maxy y))) (maxy y))]
                    )
    in
    List.foldl
        (\machine plane ->
            let (DVec fx fy fz) = minimum plane in
            machine
            |> MS.navigateTo (trace ("simplePlotPlane " ++ (show plane)) (DVec fx (fy+1) fz))
            |> simplePlotPlane ci plane targetModel
        )
        machine
        planes
    
{- Paint a single cube with energy on.
 - We assume that each y plane is completed before the next one is started.
 -}
backupPaintCube :: CubeID -> WorldShapes -> ModelTree -> Machine -> Machine
backupPaintCube ci@(CubeID cvec@(DVec cx cy cz)) (WorldShapes shapes) targetModel mach =
    let
        n = MT.cube targetModel
        u = (DVec (cx+n) (cy+n) (cz+n))
        mtbound = bound targetModel
        wshapes =
            Map.lookup ci shapes
            |> optionDefault Map.empty
        yAbove y =
            let ym = y - (mod y n) + n in
            if ym >= mtbound then
                mtbound - 1
            else
                ym

        (DVec x y z) = MS.getAt (MS.getPrintHead mach)
                
        (DVec fx fy fz) =
            (MT.scanForFirstGrounded cvec (DVec (cx+n) (cy+n) (cz+n)) targetModel)
            |> optionDefault (DVec cx (cy+1) cz)

        aboveLocation = DVec x (yAbove y) z
        aboveOtherCubeLocation = DVec fx (yAbove fy) fz
        startPlottingLocation = DVec fx fy fz
                                
        startPlotting =
            mach
            |> MS.navigateTo aboveLocation
            |> MS.navigateTo aboveOtherCubeLocation
            |> MS.navigateTo startPlottingLocation
               
        allVoxels = Map.foldl Set.union Set.empty (trace ("shapes for " ++ (show ci) ++ " are " ++ (show wshapes)) wshapes)
    in            
    simplePlotPlanes
        ci (trace ("allVoxels " ++ (show allVoxels)) allVoxels) targetModel startPlotting


fillSlot dv@(DVec dx dy dz) machine =
    let
        at@(DVec ax ay az) = MS.getAt (MS.getPrintHead machine)
        ox = dx - ax
        oy = dy - ay
        oz = dz - az
    in
    MS.executeCommands (trace ("fillSlot " ++ (show dv)) [Fill (ND ox oy oz)]) machine
        
tryToPlaceBlock dv@(DVec dx dy dz) machine =
    let
        at = MS.getAt (MS.getPrintHead machine)
        tree = MS.getTree machine
        neighborsRaw = MT.neighborMoves tree dv
        neighborsFree =
            List.filter (\n@(DVec nx ny nz) -> ny <= dy && not (lookupTree n tree)) neighborsRaw
    in
    tryPaths neighborsFree
    where
      tryPaths np =
          case np of
            [] -> Nothing
            hd : tl ->
                let
                    m = MS.navigateTo hd machine
                    navigated = MS.getAt (MS.getPrintHead m)
                in
                if navigated == hd then
                    Just (fillSlot dv m)
                else
                    tryPaths tl

accretePlanes :: CubeID -> Set DVec -> ModelTree -> Machine -> Maybe Machine
accretePlanes ci@(CubeID cvec@(DVec cx cy cz)) voxels targetModel machine =
    let
        n = MT.cube targetModel
        tree = MS.getTree machine
        mtbound = bound targetModel
        dv@(DVec x y z) = MS.getAt (MS.getPrintHead machine)
        maxy y =
            let yn = y - (mod y n) in
            if yn + n >= mtbound then
                mtbound - 2
            else
                yn + n

        {- Voxels is all the voxels we need to fill.  Find the lowest one in y that has a
         - neighbor in machine's tree.
         -}
        vlist =
            Set.filter
               (\v ->
                    let
                        neighborsRaw = MT.neighborMoves tree v
                        neighborsLowEnergy =
                            Set.fromList (List.filter (\n -> lookupTree n tree) neighborsRaw)
                        presentInTree = lookupTree v tree
                    in
                    (not presentInTree && neighborsLowEnergy /= Set.empty)
               )
               voxels
            |> Set.map YUpOrder
            |> Set.toAscList
            |> List.map (\(YUpOrder x) -> x)
    in
    case vlist of
      hd : tl ->
          machine
          |> tryToPlaceBlock hd
          |> optionThen (accretePlanes ci voxels targetModel)
      _ -> Just machine
    
{- Paint a single cube with energy off.
 - We do this by trying to add one cube at a time until the cube is finished.
 -}
paintCube :: CubeID -> ModelTree -> Map WShapeID (Set DVec) -> Machine -> Maybe Machine
paintCube ci@(CubeID cvec@(DVec cx cy cz)) targetModel wshapes machine =
    let
        n = MT.cube targetModel
        u = (DVec (cx+n) (cy+n) (cz+n))
        mtbound = bound targetModel

        (DVec x y z) = MS.getAt (MS.getPrintHead machine)
                
        allVoxels = Map.foldl Set.union Set.empty wshapes
    in            
    accretePlanes ci allVoxels targetModel machine
