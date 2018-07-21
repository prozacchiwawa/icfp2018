module Main where

import Data.Bits
import Data.Word
import qualified Data.Maybe as Maybe    
import qualified Data.ByteString as B
import qualified System.Environment as SysEnv
import qualified Data.Octree as O

import Types
import Cubes
import qualified ModelTree as MT
import qualified SQLiteTest as SQL
    
decodeLMove b f n =
    let c = intcoerce (B.index f (n+1)) in
    let sld2_a = (shift b (-6)) .&. 3 :: Int in
    let sld1_a = (shift b (-4)) .&. 3 :: Int in
    let sld2_i = (shift c (-4)) .&. 15 :: Int in
    let sld1_i = (c .&. 15) :: Int in do
      sld1 <- makeSLD sld1_a sld1_i
      sld2 <- makeSLD sld2_a sld2_i
      pure (n+2, LMove sld1 sld2)

decodeFusionP :: Int -> B.ByteString -> Int -> Maybe (Int, TraceCommand)
decodeFusionP b f n =
    let datum = (shift b (-5)) .&. 7 in
    Just (n+1, FusionP (decodeND datum))
       
decodeFusionS b f n =
    let datum = (shift b (-5)) .&. 7 in
    Just (n+1, FusionS (decodeND datum))
       
decodeFission b f n =
    let idx = B.index f (n+1) in
    let datum = (shift b (-5)) .&. 7 in
    Just (n+2, Fission (intcoerce idx) (decodeND datum))
       
decodeFill b f n =
    let datum = (shift b (-3)) .&. 31 in
    Just (n+1, Fill (decodeND datum))
      
getTraceCommand :: B.ByteString -> Int -> Maybe (Int, TraceCommand)
getTraceCommand f n =
    let b = intcoerce (B.index f n) in
    if b == 255 then
        Just (n+1, Halt)
    else if b == 254 then
        Just (n+1, Wait)
    else if b == 253 then
        Just (n+1, Flip)
    else if b .&. 15 == 4 then
        decodeSMove b f n
    else if b .&. 15 == 12 then
        decodeLMove b f n
    else if b .&. 7 == 7 then
        decodeFusionP b f n
    else if b .&. 7 == 6 then
        decodeFusionS b f n
    else if b .&. 7 == 5 then
        decodeFission b f n
    else if b .&. 7 == 3 then
        decodeFill b f n
    else
        Nothing
        
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

      "coord-to-addr-test" : (filename : tl) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        putStrLn ("offset 0 0 0 = " ++ (show (MT.bitByteOfCoord 1 (MT.bound tree) 0 0 0)))
        putStrLn ("offset 2 0 2 = " ++ (show (MT.bitByteOfCoord 1 (MT.bound tree) 2 0 2)))
        putStrLn ("offset 2 1 2 = " ++ (show (MT.bitByteOfCoord 1 (MT.bound tree) 2 1 2)))
        putStrLn ("offset 0 2 0 = " ++ (show (MT.bitByteOfCoord 1 (MT.bound tree) 0 2 0)))
        putStrLn ("offset 1 3 0 = " ++ (show (MT.bitByteOfCoord 1 (MT.bound tree) 1 3 0)))
        runSubCommands tl
                       
      "dump" : (filename : tl) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        putStrLn (show tree)
        runSubCommands tl
                       
      "run" : (filename : tl) -> do
        file <- B.readFile filename
        let tree = MT.makeTree 8 file
        --let gd = MT.scanForFirstGrounded tree
        --putStrLn ("firstGrounded " ++ (show gd))

        {- First try: flip, draw in each 6x6 interior cube, then finish the cubes in order. -}
        let c = Cubes.doCubes tree
        putStrLn ("cubes " ++ (show c))

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
