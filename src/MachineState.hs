module MachineState where

import Debug.Trace
import qualified Data.Set as Set
import Data.Set (Set)
    
import Types
import qualified Moves as M
import qualified ModelTree as MT
    
initMachine :: ModelTree -> Machine
initMachine mt =
    Machine
    { ph = PrintHead { at = DVec 0 0 0, volatile = Set.empty }
    , tree = MT.emptyTree (MT.cube mt) (bound mt)
    , heater = False
    , commands = []
    , outcome = Nothing
    }

getAt :: PrintHead -> DVec
getAt ph = at ph

setAt :: DVec -> PrintHead -> PrintHead
setAt at ph = ph { at = at }

getVolatile :: PrintHead -> Set DVec
getVolatile ph = volatile ph
              
setVolatile :: Set DVec -> PrintHead -> PrintHead
setVolatile v ph =
    ph { volatile = v }

getPrintHead :: Machine -> PrintHead
getPrintHead mach =
    ph mach
       
setPrintHead :: PrintHead -> Machine -> Machine
setPrintHead ph machine =
    machine { ph = ph }

getHeater :: Machine -> Bool
getHeater machine = heater machine
            
setHeater :: Bool -> Machine -> Machine
setHeater h machine =
    machine { heater = h }

getTree :: Machine -> ModelTree
getTree mach = tree mach

setTree :: ModelTree -> Machine -> Machine
setTree mt machine = machine { tree = mt }

getCommands :: Machine -> [TraceCommand]
getCommands mach = commands mach

newCommand :: TraceCommand -> Machine -> Machine
newCommand t machine = machine { commands = [t]++(getCommands machine) }
                             
coveredMove :: [DVec] -> Axis -> Int -> DVec -> Set DVec
coveredMove res axis dist dv@(DVec x y z) =
    if dist == 0 then
        Set.fromList res
    else
        if dist > 0 then
            case axis of
              X -> coveredMove ([DVec (x+1) y z] ++ res) axis (dist-1) dv
              Y -> coveredMove ([DVec x (y+1) z] ++ res) axis (dist-1) dv
              Z -> coveredMove ([DVec x y (z+1)] ++ res) axis (dist-1) dv
        else
            case axis of
              X -> coveredMove ([DVec (x-1) y z] ++ res) axis (dist+1) dv
              Y -> coveredMove ([DVec x (y-1) z] ++ res) axis (dist+1) dv
              Z -> coveredMove ([DVec x y (z-1)] ++ res) axis (dist+1) dv

addMove :: Axis -> Int -> DVec -> DVec
addMove axis dist (DVec x y z) =
    case axis of
      X -> (DVec (x + dist) y z)
      Y -> (DVec x (y + dist) z)
      Z -> (DVec x y (z + dist))
                   
doSMove (LLD axis dist) head =
    let at = getAt head in
    let volatile = coveredMove [at] axis dist at in
    let newAt = addMove axis dist at in
    head
    |> setAt newAt
    |> setVolatile volatile

navigateTo :: DVec -> Machine -> Machine
navigateTo aboveLocation mach =
    let
        whereWas = getAt (getPrintHead mach)
        mt = getTree mach
        (newLocation, trace) =
           fmap
               (M.pathCommands whereWas)
               (M.createPathThroughSpace
                 mt
                     whereWas
                     aboveLocation
               )
           |> optionDefault (whereWas, [])
    in
    executeCommands trace mach

executeCommands_ :: [TraceCommand] -> Machine -> Machine
executeCommands_ tracecmds machine =
    let 
        newMachine cmd =
            case cmd of
              Halt -> machine
              Wait ->
                  machine
                  |> setPrintHead (setVolatile Set.empty (getPrintHead machine))
              Flip ->
                  machine
                  |> setPrintHead (setVolatile Set.empty (getPrintHead machine))
                  |> setHeater (not (getHeater machine))
              SMove lld ->
                  let newHead = doSMove lld (getPrintHead machine) in
                  setPrintHead newHead machine
              LMove (SLD sld1_a sld1_i) (SLD sld2_a sld2_i) ->
                  let
                      head = getPrintHead machine
                      firstMoveHead = doSMove (LLD sld1_a sld1_i) head
                      secondMoveHead = doSMove (LLD sld2_a sld2_i) head
                      firstVolatile = getVolatile firstMoveHead
                      secondVolatile = getVolatile secondMoveHead
                      newHead =
                          setVolatile
                              (Set.union firstVolatile secondVolatile) secondMoveHead
                  in
                  setPrintHead newHead machine
              Fill (ND x y z) ->
                  let
                      newTree =
                          MT.addFilled
                                (addVec (getAt (getPrintHead machine)) (DVec x y z))
                                (getTree machine)
                                
                      newHead = setVolatile Set.empty (getPrintHead machine)
                  in
                  machine
                  |> setTree newTree
                  |> setPrintHead newHead
                   
              Fission _ _ -> machine
              FusionP _ -> machine
              FusionS _ -> machine

        traceMsg hd = "cmd " ++ (show (getAt (getPrintHead machine))) ++ " : " ++ show hd
    in
    case tracecmds of
      hd : tl ->
          let
              tlx = trace (traceMsg hd) tl :: [TraceCommand]
          in
          executeCommands
             tlx
             (newCommand hd (newMachine hd))
      _ -> machine

executeCommands :: [TraceCommand] -> Machine -> Machine
executeCommands tl machine =
    case outcome machine of
      Just o -> machine
      Nothing -> executeCommands_ tl machine

abort s machine = machine { outcome = Just s }
