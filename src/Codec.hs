module Codec where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B

import Types
    
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
