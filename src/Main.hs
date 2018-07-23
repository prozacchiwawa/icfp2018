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
import qualified MachineState as MS
    
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
        let rgns = R.getShapesInCube (CubeID (DVec 0 0 0)) tree
        let labels = R.getRegionLabels rgns
        putStrLn ("rgns " ++ (show labels))
        let shapes = R.shapeFromRegions rgns tree
        putStrLn ("shapes " ++ (show shapes))

      "cube-regions" : (filename : tl) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let extractedCubes = Cubes.extract tree
        let wshapes = Cubes.getWorldShapes extractedCubes

        let connectome = Cubes.getConnectome wshapes tree

        let grounded = Cubes.groundedShapeSet wshapes

        putStrLn ("grounded " ++ (show grounded))
        putStrLn (show connectome)

        runSubCommands tl

      "skeleton" : (filename : (outf : tl)) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let extractedCubes = Cubes.extract tree
        let wshapes = Cubes.getWorldShapes extractedCubes

        let connectome = Cubes.getConnectome wshapes tree

        let grounded = Cubes.groundedShapeSet wshapes

        let ct = pathThroughShapes grounded connectome
        let skeleton = drawPathsToShapes ct grounded connectome wshapes tree

        let skeletonTree = MT.addFilledSet skeleton (MT.emptyTree (MT.cube tree) (bound tree))
        let skExtractedCubes = Cubes.extract skeletonTree
        let skWshapes = Cubes.getWorldShapes skExtractedCubes
        let cubes = Cubes.cubesInBasicOrder wshapes
        let machine =
                List.foldl
                        (\machine (ci,csh) ->
                             CQ.backupPaintCube
                                   (trace ("cube " ++ (show ci)) ci) skWshapes skeletonTree machine
                        )
                        (MS.executeCommands [Flip] (MS.initMachine skeletonTree))
                        cubes
                        
        let postMachine =
                machine
                |> MS.executeCommands [Flip]

        {- Try to place the blocks. -}
        let machineAfterPlacing =
                List.foldl
                    (\machine (ci,csh) ->
                         let
                             (WorldShapes ws) = wshapes
                             cubeShapes =
                                 Map.lookup ci ws
                                 |> optionDefault Map.empty
                         in
                         CQ.paintCube ci tree cubeShapes machine
                         |> optionDefault machine
                    )
                    postMachine
                    cubes

        let finMachine =
                machineAfterPlacing
                |> MS.navigateTo (DVec 0 0 0)
                |> MS.executeCommands [Halt]
                   
        B.writeFile outf (encodeTraceCommands (List.reverse (MS.getCommands finMachine)))
            
        runSubCommands tl
                 
      -- Time to finish at least one strategy completely
      "type1" : (filename : (outf : tl)) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        let extractedCubes = Cubes.extract tree
        let wshapes = Cubes.getWorldShapes extractedCubes
        let emt = MT.emptyTree 8 (bound tree)
        let cubes = Cubes.cubesInBasicOrder wshapes
        let machine =
                List.foldl
                        (\machine (ci,csh) ->
                             CQ.backupPaintCube
                                   (trace ("cube " ++ (show ci)) ci) wshapes tree machine
                        )
                        (MS.executeCommands [Flip] (MS.initMachine tree))
                        cubes
                        
        let finMachine =
                machine
                |> MS.executeCommands [Flip]
                |> MS.navigateTo (DVec 0 0 0)
                |> MS.executeCommands [Halt]

        B.writeFile outf (encodeTraceCommands (List.reverse (MS.getCommands finMachine)))
        runSubCommands tl

      "sqlite-test" : (filename : tl) -> do
        SQL.runDB
        runSubCommands tl

      "emptytree" : tl -> do
        let mt = MT.emptyTree 8 10
        putStrLn ("mt 0 0 0 -> " ++ (show (lookupTree (DVec 0 0 0) mt)))
                       
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
