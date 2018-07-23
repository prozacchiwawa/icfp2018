module Cubes where

import Debug.Trace
import Data.Bits
import Data.Word
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString as B
import qualified System.Environment as SysEnv
import qualified Data.Octree as O

import Types
import qualified Moves as M
import qualified ModelTree as MT
import qualified Region as R
import qualified MachineState as MS
    
isNonEmptyCube ci mt =
    MT.cubeFoldZXY
        ci
        (\dv a ->
             if lookupTree dv mt then
                 True
             else
                 a
        )
        False
        mt
    
doCubes_ acc f ci@(CubeID (DVec x y z)) mt =
    let n = bound mt in
    let s = MT.cube mt in
    let u = CubeID (DVec x y (z+s)) in
    if z >= n then
        doCubes_ acc f (CubeID (DVec (x+s) y 0)) mt
    else if x >= n then
        doCubes_ acc f (CubeID (DVec 0 (y+s) 0)) mt
    else if y >= n then
        acc
    else if isNonEmptyCube ci mt then
        doCubes_ (f ci acc) f u mt
    else
        doCubes_ acc f u mt

doCubes :: ModelTree -> Set CubeID
doCubes mt =
    doCubes_ Set.empty Set.insert (CubeID (DVec 0 0 0)) mt

neighborCubes :: CubeID -> Int -> ModelTree -> [CubeID]
neighborCubes (CubeID (DVec x y z)) s mt =
    let
        bounds = bound mt
        toLeft =  DVec (x-s) y     z
        toRight = DVec (x+s) y     z
        below =   DVec x     (y-s) z
        above =   DVec x     (y+s) z
        ahead =   DVec x     y     (z-s)
        behind =  DVec x     y     (z+s)
        possible =
            [ CubeID toLeft
            , CubeID toRight
            , CubeID above
            , CubeID below
            , CubeID ahead
            , CubeID behind ]
    in
    List.filter
        (\(CubeID at@(DVec x y z)) ->
             x >= 0 && y >= 0 && z >= 0 &&
             x < bounds && y < bounds && z < bounds
        )
        possible

extract :: ModelTree -> ExtractedCubes
extract tree =
    let c = Cubes.doCubes tree in
    ExtractedCubes
    (List.foldl
        (\m (CubeID c) ->
             Map.insert
                    (CubeID c)
                    tree
                    m
        )
        Map.empty
        (Set.toList c)
    )
        
getWorldShapes :: ExtractedCubes -> WorldShapes
getWorldShapes (ExtractedCubes extractedCubes) =
    WorldShapes
    (Map.mapWithKey
           (\k v ->
                let
                    regions = R.getShapesInCube k v
                    shapes = R.shapeFromRegions regions v
                in
                shapes
           )
           extractedCubes
    )

getShapeSet :: WShapeID -> WorldShapes -> Set DVec
getShapeSet sid (WorldShapes wshapes) =
    Map.lookup (cubeIDFromWShapeID sid) wshapes
    |> optionThen (\cmap -> Map.lookup sid cmap)
    |> optionDefault Set.empty
           
{- Find the grounded shapes -}
groundedShapes :: Map WShapeID (Set DVec) -> Map WShapeID Bool
groundedShapes shapes =
    Map.mapWithKey
       (\k v ->
            (List.filter
                (\dv@(DVec x y z) -> y == 0)
                (Set.toList v))
            /= []
       )
       shapes

shellShape :: Int -> Map WShapeID (Set DVec) -> Map WShapeID (Set DVec)
shellShape n shapes =
    Map.mapWithKey
       (\k v ->
            Set.fromList
               (List.filter
                    (\dv@(DVec x y z) ->
                         let
                             nn = n - 1
                             xmn = mod x n 
                             ymn = mod y n
                             zmn = mod z n
                      in
                      xmn == 0 || ymn == 0 || zmn == 0 || xmn == nn || ymn == nn || zmn == nn
                    )
                    (Set.toList v)
               )
       )
       shapes

{- Find the neighbors of the shape shell that lie outside the same cube. -}
externalNeighbors :: Int -> Map WShapeID (Set DVec) -> ModelTree -> Map WShapeID (Set DVec)
externalNeighbors n shapes mt =
    Map.mapWithKey
       (\(WShapeID k@(DVec x y z)) v ->
            let
                tcx = x - (mod x 8)
                tcy = y - (mod y 8)
                tcz = z - (mod z 8)
                outsideX = Set.fromList [tcx - 1, tcx + n]
                outsideY = Set.fromList [tcy - 1, tcy + n]
                outsideZ = Set.fromList [tcz - 1, tcz + n]
                allNeighbors =
                    List.foldl
                        (\s n ->
                             let
                                 neighbors = MT.neighborMoves mt n
                                 outsideNeighbors =
                                     List.filter
                                         (\n@(DVec x y z) ->
                                              Set.member x outsideX ||
                                                 Set.member y outsideY ||
                                                 Set.member z outsideZ
                                         )
                                         neighbors
                             in
                             Set.union (Set.fromList outsideNeighbors) s
                        )
                        Set.empty
                        (Set.toList v)
            in
            allNeighbors
       )
       shapes
        
{- Adjacent shapes 
 - Given the position and shapes of cube 1 and position and shapes of cube2,
 - output a list of connected shapes from c1 to c2.
 -}
adjacentShapes :: Int -> Map WShapeID (Set DVec) -> Map WShapeID (Set DVec) -> ModelTree -> Map WShapeID [(WShapeID,WShapeID)]
adjacentShapes n c1 c2 mt =
    let
        {- shellsC1 is a shape-identifier-indexed map of potentially overlapping points for
         - other cubes.
         - If one of these appears in a shape in another cube then the shapes are connected.
         -}
        shellsC1 = externalNeighbors n (shellShape n c1) mt
        c2List = Map.toList c2

        c2ListMatchingShellsC1 =
            Map.mapWithKey
               (\k v ->
                    let
                        matchingListFromC2 =
                            List.map
                                (\(k,i) ->  (k,Set.intersection v i))
                                c2List

                        onlyMatchingC2 =
                            List.filter (\(k,i) -> Set.size i > 0) matchingListFromC2
                    in
                    List.map (\(c2k,i) -> (k,c2k)) onlyMatchingC2
               )
               shellsC1
    in
    Map.filter (\v -> List.length v > 0) c2ListMatchingShellsC1

groundedShapeSet :: WorldShapes -> Set WShapeID
groundedShapeSet (WorldShapes wshapes) =
    let
        groundedShapesInCubes =
            Map.mapWithKey
                   (\k v ->
                        Cubes.groundedShapes v
                   )
                   wshapes

        groundedShapes = Map.elems groundedShapesInCubes
        grounded =
            List.map
                    (\(k,v) -> k)
                    (List.filter (\(k,v) -> v)
                             (List.concat (List.map Map.toList groundedShapes))
                    )
    in
    Set.fromList grounded

cubesInBasicOrder :: WorldShapes -> [(CubeID,Map WShapeID (Set DVec))]
cubesInBasicOrder (WorldShapes wshapes) =
    List.sortOn
            (\(k,v) -> BasicCubeIDOrder k)
            (Map.toList wshapes)
       
getConnectome :: WorldShapes -> ModelTree -> Connectome
getConnectome (WorldShapes wshapes) mt =
    let
        connectomeRaw =
            List.map
                    (\(k1,v1) ->
                         List.map
                                 (\(k2,v2) ->
                                      if k1 < k2 then
                                          [adjacentShapes 8 v1 v2 mt]
                                      else
                                          []
                                 )
                                 (Map.toList wshapes)
                    )
                    (Map.toList wshapes)
    in
    Connectome
      (List.filter (\m -> (Map.size m) > 0) (List.concat (List.concat connectomeRaw)))

neighborShapes :: WShapeID -> Connectome -> Set WShapeID
neighborShapes shape (Connectome connectome) =
    List.foldl
        Set.union
        Set.empty
        (List.map smallNeighborShapes connectome)
    where
    smallNeighborShapes m =
        case Map.lookup shape m of
          Just plist -> Set.fromList (List.map (\(_,v) -> v) plist)
          Nothing -> Set.empty

setOfConnectome :: ConnectomeTree -> Set WShapeID
setOfConnectome (ConnectomeTree shp lst) =
    List.foldl
        (\a b -> Set.union a (setOfConnectome b))
        (Set.singleton shp)
        lst

accumBranches ::
    ConnectomeTree ->
    Set WShapeID ->
    [WShapeID] ->
    Connectome ->
    ConnectomeTree
accumBranches ct@(ConnectomeTree here branches) used ls connectome =
    case ls of
      [] -> ct
      hd : tl ->
          let
              newUsed = Set.union used (setOfConnectome ct)
              newConnectome = pathThroughShapes_ hd newUsed connectome :: ConnectomeTree
          in
          accumBranches
              (ConnectomeTree here ([newConnectome] ++ branches))
              newUsed
              tl
              connectome

pathThroughShapes_ ::
    WShapeID ->
    Set WShapeID ->
    Connectome ->
    ConnectomeTree
pathThroughShapes_ at used connectome =
    let
        neighbors = neighborShapes at connectome
        firstFront = Set.toList (Set.difference neighbors used)
    in
    case firstFront of
      [] -> ConnectomeTree at []
      ls -> accumBranches (ConnectomeTree at []) used ls connectome

pathThroughShapes :: Set WShapeID -> Connectome -> ConnectomeTree
pathThroughShapes grounded connectome =
    accumBranches 
        (ConnectomeTree (WShapeID (DVec 0 0 0)) [])
        Set.empty
        (Set.toList grounded)
        connectome

connectomeShapeID :: ConnectomeTree -> WShapeID
connectomeShapeID (ConnectomeTree ct _) = ct

{- Given the set of points in a shape, select a point from the =0 plane and return a route
 - through matter between the point referenced by the shapeid and the ground point.
 -}
groundOneShape :: ModelTree -> WShapeID -> Set DVec -> Set DVec
groundOneShape mt (WShapeID sid) shape =
    let
        -- Choose a point at plane 0
        plane0 = Set.filter (\(DVec x y z) -> y == 0) shape
        chosenGround =
            case Set.toList plane0 of
              hd : tl -> hd
              _ -> sid

        matterPath = M.createPathThroughMatter mt chosenGround sid
    in
    matterPath
    |> fmap Set.fromList
    |> fmap (Set.insert chosenGround)
    |> optionDefault Set.empty

connectTwoShapes :: ModelTree -> WShapeID -> WShapeID -> Set DVec
connectTwoShapes mt (WShapeID sid) (WShapeID eid) =
    let
        matterPath = M.createPathThroughMatter mt sid eid
    in
    matterPath
    |> fmap Set.fromList
    |> fmap (Set.insert sid)
    |> optionDefault Set.empty
       
drawPathsToShapes :: ConnectomeTree ->    Set WShapeID -> Connectome -> WorldShapes ->       ModelTree -> Set DVec
drawPathsToShapes (ConnectomeTree tr@(WShapeID wid) lst) grounded        connectome    ws@(WorldShapes shapes) targetModel =
    if tr == WShapeID (DVec 0 0 0) then
        -- Ground the shapes in lst
        List.foldl
            (\fig ctree ->
                 let
                     sid = connectomeShapeID ctree
                     shapeSet =
                         shapes
                         |> Map.lookup (cubeIDFromWShapeID sid)
                         |> optionThen (Map.lookup sid)
                         |> optionDefault Set.empty

                     groundSnake = groundOneShape targetModel sid shapeSet
                 in
                 Set.union
                    (drawPathsToShapes ctree grounded connectome ws targetModel)
                    (Set.union groundSnake fig)
            )
            Set.empty
            lst
    else
        List.foldl
            (\fig ctree ->
                 let
                     sid = connectomeShapeID ctree
                     shapeSet =
                         shapes
                         |> Map.lookup (cubeIDFromWShapeID sid)
                         |> optionThen (Map.lookup sid)
                         |> optionDefault Set.empty

                     connectSnake = connectTwoShapes targetModel (WShapeID wid) sid
                 in
                 Set.union
                        (drawPathsToShapes ctree grounded connectome ws targetModel)
                        (Set.union connectSnake fig)
            )
            Set.empty
            lst

{- Evaluate the goal of having a cube filled in the same way as the map specifies 
 - Set is non-empty if they don't match.
 -}
isFilledSameWay :: CubeID -> ModelTree -> ModelTree -> Set DVec
isFilledSameWay cid modelTarget machineTree =
    let
        n = MT.cube modelTarget
    in
    MT.cubeFoldZXY
        cid
        (\dv a ->
             if lookupTree dv machineTree /= lookupTree dv modelTarget then
                 Set.insert dv a
             else
                 a
        )
        Set.empty
        modelTarget

            
