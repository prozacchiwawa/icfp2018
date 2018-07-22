module Codec where

import Data.Bits
import Data.Word
import qualified Data.List as List
import qualified Data.ByteString as B

import Types
    
makeSLD a i =
    case a of
      1 -> Just (SLD X (i - 5))
      2 -> Just (SLD Y (i - 5))
      3 -> Just (SLD Z (i - 5))
      _ -> Nothing

sld2Val (SLD a i) =
    let
        aout =
            case a of
              X -> 1
              Y -> 2
              Z -> 3
        iout =
            mod (i+5) 16
    in
    (aout,iout)

lld2Val (LLD a i) =
    let aout = 
            case a of
              X -> 1
              Y -> 2
              Z -> 3
        iout =
            mod (i+15) 32
    in
    (aout,iout)

nd2Val (ND dx dy dz) =
    (dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)
    
makeLLD a i =
    case a of
      1 -> Just (LLD X (i - 15))
      2 -> Just (LLD Y (i - 15))
      3 -> Just (LLD Z (i - 15))
      _ -> Nothing
      
decodeND datum =
    let z = mod datum 3 :: Int in
    let xy = div datum 3 :: Int in
    let y = mod xy 3 :: Int in
    let x = mod (div xy 3) 3 :: Int in
    ND (x - 1) (y - 1) (z - 1)

decodeLMove b f n =
    let c = intcoerce (B.index f (n+1)) in
    let sld2_a = (shift b (-6)) .&. 3 :: Int in
    let sld1_a = (shift b (-4)) .&. 3 :: Int in
    let sld2_i = (shift c (-4)) .&. 15 :: Int in
    let sld1_i = (c .&. 15) :: Int in do
      sld1 <- makeSLD sld1_a sld1_i
      sld2 <- makeSLD sld2_a sld2_i
      pure (n+2, LMove sld1 sld2)

encodeLMove :: SLD -> SLD -> B.ByteString
encodeLMove sld1 sld2 =
    let
        (sld1_a,sld1_i) = sld2Val sld1
        (sld2_a,sld2_i) = sld2Val sld2
    in
    B.pack
         [ toEnum (12 + (shift sld1_a 6) + (shift sld2_a 4))
         , toEnum ((shift sld1_i 4) + (shift sld2_i 4))
         ]

decodeSMove :: Int -> B.ByteString -> Int -> Maybe (Int, TraceCommand)
decodeSMove b f n =
    let c = B.index f (n+1) in
    let lld_a = shift b (-4) .&. 3 in
    let lld_i = c .&. 31 in do
      lld <- makeLLD lld_a (fromInteger (toInteger lld_i))
      pure (n+2, SMove lld)

encodeSMove :: LLD -> B.ByteString
encodeSMove lld =
    let (lld_a, lld_i) = lld2Val lld in
    B.pack
         [ toEnum (4 + (shift lld_a 4)), toEnum lld_i ]
           
decodeFusionP :: Int -> B.ByteString -> Int -> Maybe (Int, TraceCommand)
decodeFusionP b f n =
    let datum = (shift b (-5)) .&. 7 in
    Just (n+1, FusionP (decodeND datum))

encodeFusionP :: ND -> B.ByteString
encodeFusionP nd =
    B.pack
         [ toEnum (7 + (shift (nd2Val nd) 3)) ]

decodeFusionS b f n =
    let datum = (shift b (-5)) .&. 7 in
    Just (n+1, FusionS (decodeND datum))

encodeFusionS nd =
    B.pack
         [ toEnum (6 + (shift (nd2Val nd) 3)) ]

decodeFission b f n =
    let idx = B.index f (n+1) in
    let datum = (shift b (-5)) .&. 7 in
    Just (n+2, Fission (intcoerce idx) (decodeND datum))

encodeFission :: Int -> ND -> B.ByteString
encodeFission id nd =
    B.pack
         [ toEnum (5 + (shift (nd2Val nd) 3)), toEnum id ]
         
decodeFill b f n =
    let datum = (shift b (-3)) .&. 31 in
    Just (n+1, Fill (decodeND datum))

encodeFill nd =
    B.pack
         [ toEnum (3 + (shift (nd2Val nd) 3)) ]
      
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

encodeTraceCommand :: TraceCommand -> B.ByteString
encodeTraceCommand t =
    case t of
      Halt -> B.pack [toEnum 255]
      Wait -> B.pack [toEnum 254]
      Flip -> B.pack [toEnum 253]
      SMove lld -> encodeSMove lld
      LMove sld1 sld2 -> encodeLMove sld1 sld2
      Fission id nd -> encodeFission id nd
      Fill nd -> encodeFill nd
      FusionP nd -> encodeFusionP nd
      FusionS nd -> encodeFusionS nd
              
encodeTraceCommands :: [TraceCommand] -> B.ByteString
encodeTraceCommands cmds =
    B.intercalate (B.pack []) (List.map encodeTraceCommand cmds)
