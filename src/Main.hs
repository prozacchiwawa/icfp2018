module Main where

import Debug.Trace
import Data.Bits
import Data.Word
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString as B
import qualified System.Environment as SysEnv
import qualified Data.Octree as O

import Types
import Cubes
import Codec
import qualified ModelTree as MT
import qualified SQLiteTest as SQL
import qualified Region as R
import qualified Moves as M
import qualified CommandQ as CQ
            
listOutTraceCommands :: Int -> Int -> B.ByteString -> IO ()
listOutTraceCommands n l f =
    if n >= l then do
        pure ()
    else
      case getTraceCommand f n of
        Just (nn, c) -> do
            putStrLn (show c)
            listOutTraceCommands nn l f
        Nothing -> do
            putStrLn "**error**"

runSubCommands :: [String] -> IO ()
runSubCommands args =
    case args of
      "decodeTrace" : (filename : tl) -> do
        file <- B.readFile filename
        let l = B.length file
        listOutTraceCommands 0 l file
        runSubCommands tl

      "dump" : (filename : tl) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        putStrLn (show tree)
        runSubCommands tl

      "findPath" : (filename : (xstr : (ystr : (zstr : tl)))) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let x = read xstr :: Int
        let y = read ystr :: Int
        let z = read zstr :: Int
        let p = M.createPathThroughSpace tree (DVec x y z) (DVec 0 0 0)
        putStrLn ("path " ++ (show p))

      "findPath2" : (filename : (sxstr : (systr : (szstr : (exstr : (eystr : (ezstr : tl))))))) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let sx = read sxstr :: Int
        let sy = read systr :: Int
        let sz = read szstr :: Int
        let ex = read exstr :: Int
        let ey = read eystr :: Int
        let ez = read ezstr :: Int
        let p = M.createPathThroughSpace tree (DVec sx sy sz) (DVec ex ey ez)
        putStrLn ("path " ++ (show p))

      "test-region" : (filename : tl) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let rgns = R.getShapesInCube tree
        let labels = R.getRegionLabels rgns
        putStrLn ("rgns " ++ (show labels))
        let shapes = R.shapeFromRegions rgns tree
        putStrLn ("shapes " ++ (show shapes))

      "cube-regions" : (filename : tl) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let c = Cubes.doCubes tree
        let extractedCubes =
                List.foldl
                        (\m (CubeID c) ->
                             Map.insert
                                (CubeID c)
                                (MT.extractCube c 8 tree)
                                m
                        )
                        Map.empty
                        (Set.toList c)
        let shapes =
                Map.mapWithKey
                     (\k v ->
                          let
                              regions = R.getShapesInCube v
                              shapes = R.shapeFromRegions regions v
                          in
                          shapes
                     )
                     extractedCubes

        let wshapes = getWorldShapes shapes

        let connectome = Cubes.getConnectome wshapes tree

        let grounded = Cubes.groundedShapeSet wshapes

        putStrLn ("grounded " ++ (show grounded))
        putStrLn (show connectome)

        runSubCommands tl

      "skeleton" : (filename : (outfile : tl)) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let c = Cubes.doCubes tree
        let extractedCubes =
                List.foldl
                        (\m (CubeID c) ->
                             Map.insert
                                (CubeID c)
                                (MT.extractCube c 8 tree)
                                m
                        )
                        Map.empty
                        (Set.toList c)
        let shapes =
                Map.mapWithKey
                     (\k v ->
                          let
                              regions = R.getShapesInCube v
                              shapes = R.shapeFromRegions regions v
                          in
                          shapes
                     )
                     extractedCubes

        let wshapes = getWorldShapes shapes

        let connectome = Cubes.getConnectome wshapes tree

        let grounded = Cubes.groundedShapeSet wshapes

        putStrLn ("grounded " ++ (show grounded))
        putStrLn ("connectome " ++ (show connectome))

        let shapeID = WShapeID (DVec 8 0 7)
                 
        let neighborShapes = Cubes.neighborShapes shapeID connectome
        putStrLn ("neighbors of " ++ (show shapeID) ++ " " ++ (show neighborShapes))

        let ct = pathThroughShapes grounded connectome
        putStrLn ("paths " ++ (show ct))

        let skeleton = drawPathsToShapes ct grounded connectome
        putStrLn ("skeleton " ++ (show skeleton))
                 
        runSubCommands tl
                 
      "run" : (filename : (outfile : tl)) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        --let gd = MT.scanForFirstGrounded tree
        --putStrLn ("firstGrounded " ++ (show gd))

        {- First try: flip, draw in each 6x6 interior cube, then finish the cubes in order. -}
        let c = Cubes.doCubes tree

        let extracted = MT.extractCube (DVec 1 0 1) 8 tree
        putStrLn (show extracted)
{-
        putStrLn "flip"
        let resLoc = doPathThroughCubes mt c (DVec 0 0 0)
        let finLoc = doFinishCubes mt c resLoc
        putStrLn "flip"
        returnHome finLoc
-}
        runSubCommands tl

      -- Time to finish at least one strategy completely
      "type1" : (filename : (outf : tl)) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let c = Cubes.doCubes tree
        let extractedCubes =
                List.foldl
                        (\m (CubeID c) ->
                             Map.insert
                                (CubeID c)
                                (MT.extractCube c 8 tree)
                                m
                        )
                        Map.empty
                        (Set.toList c)
        let shapes =
                Map.mapWithKey
                     (\k v ->
                          let
                              regions = R.getShapesInCube v
                              shapes = R.shapeFromRegions regions v
                          in
                          shapes
                     )
                     extractedCubes

        let wshapes = getWorldShapes shapes
        let emt = MT.emptyTree 8 (MT.bound tree)
        let cubes = Cubes.cubesInBasicOrder wshapes
        let (lastPos, moves) =
                List.foldl
                        (\(last,cmds) (ci,csh) ->
                             let
                                 (l,c) =
                                     CQ.backupPaintCube 8 (trace ("cube " ++ (show ci)) ci) csh last emt tree
                             in
                             (l, cmds ++ c)
                        )
                        (DVec 0 0 0, [])
                        cubes
        let (finAt, finalPath) =
                fmap
                    (CQ.pathCommands lastPos)
                    (M.createPathThroughSpace tree lastPos (DVec 0 0 0))
                |> optionDefault (lastPos, [])
                    
        let moveres =
                List.concat [ [Flip], moves, [Flip], finalPath, [Halt] ]

        B.writeFile outf (encodeTraceCommands moveres)
        runSubCommands tl

      "sqlite-test" : (filename : tl) -> do
        SQL.runDB
        runSubCommands tl

      "emptytree" : tl -> do
        let mt = MT.emptyTree 8 10
        putStrLn ("mt 0 0 0 -> " ++ (show (MT.lookupTree (DVec 0 0 0) mt)))
                       
      [] -> do
        pure ()
                       
      dunno : tl -> do
        putStrLn ("Can't understand subcommand " ++ dunno)
        pure ()
    
againAndAgain :: Int -> (a -> a) -> (a -> IO ()) -> a -> IO a
againAndAgain n step output v =
    if n > 0 then do
               let updated = step v
               output updated
               againAndAgain (n-1) step output updated
    else
        do
          pure v
                      
main :: IO ()
main = do
  args <- SysEnv.getArgs
  runSubCommands args
