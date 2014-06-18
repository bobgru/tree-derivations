{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Backend.SVG
import Diagrams.Prelude
--import Diagrams.TwoD.Arrow

main    = defaultMain diagram

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

-- Not sure why, but padding the final drawing didn't work 
-- as expected, so manually create some border space by
-- including invisible extensions of the vectors.
box =  fromOffsets [v] # scale (1.1 * (1 + r)) # lw 0
    <> fromOffsets [v] # scale (-0.2) # lw 0
    <> fromOffsets [wl] # scale 1.3 # lw 0
    <> fromOffsets [wr] # scale 1.3 # lw 0

mainAxis =  dot p # fc blue <> arr p v
         <> dottedExtension

dottedExtension = fromOffsets [v] # scale r # dashing [0.02, 0.02] 0 # moveTo q 

lateralSegments =  dot q # fc blue
                <> arr q wl <> arr q wr
                <> arc (direction wr) (direction wl) # scale 0.2 # moveTo q

-- Include the alignment helpers to make it easier to fine tune the label coordinates.
labels =  position angles
       <> position points
       <> position vects
       -- <> position (zip ps   (repeat (dot # fc blue)))       -- alignment helpers
    where
        -- For the alignment helpers
        dot         = circle 0.02 # lw 0
        ps          = [ p2(0,1), p2(1,0), p2(1,1), p2(0,0) ]
        
        angles      = [ (p2(-0.15,1.25), dTheta), (p2(0.15,1.25), dTheta) ]
        dTheta      = scalarSym [thetaSymbol]
        thetaSymbol = toEnum 0X3B8 :: Char

        points      = [ (p2(0.1,0), dp), (p2(0.1,0.95), dq) ]
        dp          = scalarSym "p"
        dq          = scalarSym "q"

        vects       = [ (p2(0.1,0.5), dv), (p2(-0.4,1.18), dw "l"), (p2(0.4,1.18), dw "r") ]
        dv          = vectorSym "v"
        dw s        = vectorSym "w" # withSubscript s 0.075

        scalarSym s = text s # italic # fontSize 1 # scale 0.15
        vectorSym s = text s # bold   # fontSize 1 # scale 0.15

        withSubscript :: String -> Double -> Diagram B R2 -> Diagram B R2
        withSubscript t s d = d <> topLeftText t # italic
                                #  scale s # translate (r2(0.04, -0.02))
