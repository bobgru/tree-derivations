{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Backend.SVG
import Diagrams.Prelude
--import Diagrams.TwoD.Arrow

main       = defaultMain diagram

diagram = box <> lateralSegments <> mainAxis <> labels

p  = origin
v  = unitY
q  = p .+^ v
wl = v # scale r # rotateBy th
wr = v # scale r # rotateBy (-th)
r  = 0.7
th = (1/7)

dot     = lw 0 . place (circle 0.02)
arr p v = arrowBetween' (with & headSize .~ 0.1) p (p .+^ v)

box =  fromOffsets [v] # scale (1.1 * (1 + r)) # lw 0
    <> fromOffsets [v] # scale (-0.1) # lw 0
    <> fromOffsets [wl] # scale 1.3 # lw 0
    <> fromOffsets [wr] # scale 1.5 # lw 0

mainAxis =  dot p # fc blue <> arr p v
         <> dottedExtension

-- dottedExtension = place (fromOffsets [v]) q # dashing [0.02, 0.02] 0 -- # scale r
dottedExtension = fromOffsets [v] # scale r # dashing [0.02, 0.02] 0 # moveTo q 

lateralSegments =  dot q # fc blue
                <> arr q wl <> arr q wr
                <> arc (direction wr) (direction wl) # scale 0.2 # moveTo q

labels =  position angles
       <> position points
       <> position vects
       -- <> position (zip ps   (repeat (dot # fc blue)))       -- alignment helpers
    where
        dot  = circle 0.02 # lw 0
        ps     = [p2(0,1), p2(1,0), p2(1,1), p2(0,0)]
        angles = [ (p2(-0.15,1.25), dTheta), (p2(0.15,1.25), dTheta)]
        points = [ (p2(0.1,0), dp), (p2(0.1,0.95), dq)]
        vects  = [ (p2(0.1,0.5), dv), (p2(-0.4,1.18), dw "l"), (p2(0.4,1.18), dw "r")]
        thetaSymbol = toEnum 0X3B8 :: Char
        scalarSym s = text s # italic # fontSize 1 # scale 0.15
        vectorSym s = text s # bold   # fontSize 1 # scale 0.15
        dTheta      = scalarSym [thetaSymbol]
        dp          = scalarSym "p"
        dq          = scalarSym "q"
        dr          = scalarSym "r"
        dv          = vectorSym "v"
        dw s        = vectorSym "w" # withSubscript s 0.075

withSubscript :: String -> Double -> Diagram B R2 -> Diagram B R2
withSubscript t s d = d <> topLeftText t # italic # scale s # translate (r2(0.04, -0.02))
