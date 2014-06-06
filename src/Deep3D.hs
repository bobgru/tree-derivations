module Main where
import Data.Cross(cross3)
import Data.Tree(Tree, flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
--import Diagrams.Backend.SVG(SVG)
import Diagrams.Prelude hiding (rotationAbout, direction, angleBetween)
import Diagrams.ThreeD.Transform(aboutY, pointAt', rotationAbout)
import Diagrams.ThreeD.Types(unp3, R3, r3, T3, Spherical)
import Diagrams.ThreeD.Vector(unitZ, direction, angleBetween)

--type TreeNode3 = (P3, R3, Double)
--type TreeNode = (P2, R2, Double)
--type Dgm = Diagram SVG R2

data TreeConfig = TC {
    tcScale        :: Double,
    tcCutOff       :: Double,
    tcMinLineWidth :: Double,
    tcInitialWidth :: Double,
    tcWidthTaper   :: Double,
    tcBranchScale  :: Double,
    tcBranchAngle  :: Double
} deriving (Show)

--tc :: TreeConfig
tc = TC {
    tcScale        = s,
    tcCutOff       = 0.12 * s,
    tcMinLineWidth = 0.01 * s,
    tcInitialWidth = 0.1  * s,
    tcWidthTaper   = 0.7,
    tcBranchScale  = 0.6,
    tcBranchAngle  = 1/7
}
    where s = 10000

--main :: IO ()
main = defaultMain $ pad 1.1 $ renderTree $ buildTree

--renderTree :: Tree TreeNode3 -> Dgm
renderTree =  mconcat . flatten . fmap drawBranch . fmap projectNode

--buildTree :: Tree TreeNode3
buildTree =  unfoldTree branches seed

--seed :: TreeNode3
seed =  (origin, unitZ ^* tcScale tc, tcInitialWidth tc)

--projectNode :: TreeNode3 -> TreeNode
projectNode (p, v, w) = (p', v', w)
    where q  = p .+^ v
          q' = projectPtXZ q
          p' = projectPtXZ p
          v' = q' .-. p'

--projectPtXZ :: P3 -> P2
projectPtXZ p = case unp3 p of (x, _, z) -> p2 (x, z)

--inject :: R2 -> R3
inject v = case unr2 v of (x, y) -> r3 (x, y, 0)

--drawBranch :: TreeNode -> Dgm
drawBranch n@(_, _, w)
    | w <= tcMinLineWidth tc  =  drawUniform n
    | otherwise               =  drawTapered n

--drawUniform :: TreeNode -> Dgm
drawUniform (p, v, w) = position [(p, fromOffsets [v])] # lw w

--drawTapered :: TreeNode -> Dgm
drawTapered (p, v, w) = place taper p
    where taper = (closeLine . lineFromVertices) [ p, a, b, c, d ]
                # strokeLoop # fc black # lw 0.01
          p'  = p .+^ v
          w'  = taperWidth w
          n   = v # rotateBy (1/4) # normalized
          w2  = w  / 2 ; w2' = w' / 2
          a   = p  .-^ (w2  *^ n) ; b   = p' .-^ (w2' *^ n)
          c   = p' .+^ (w2' *^ n) ; d   = p  .+^ (w2  *^ n)

--taperWidth :: Double -> Double
taperWidth w = max (w * tcWidthTaper tc) (tcMinLineWidth tc)

--branches :: TreeNode3 -> (TreeNode3, [TreeNode3])
branches n@(_, v, _)
    | magnitude v < tcCutOff tc  =  (n, [])
    | otherwise                  =  (n, branchTips n)

-- Build a regular polygon in the XY-plane and tilt it perpendicular
-- to the vector it branches from. Orient the polygon to make the
-- projection more interesting.
--branchTips :: TreeNode3 -> [TreeNode3]
branchTips n@(_, v, _) = polygon po
                       # map (.-. origin)
                       # map inject
                       # map (^+^ (unitZ ^* h))
                       # map (transform (pointAt'' unitZ unitZ v))
                       # map (^*  (magnitude v * tcBranchScale tc))
                       # map (mkTip n)
    where po = PolygonOpts (PolyRegular c s) (OrientTo v') origin
          c  = 3            -- number of sides
          s  = 0.782        -- length of side
          v' = r2 (1,3)     -- orientation vector
          h  = 0.623        -- "height" of tips above base

-- Copied from http://projects.haskell.org/diagrams/haddock/src/
-- Diagrams-ThreeD-Transform.html#pointAt
-- and modified to change the calculation of tilt angle.
-- Also eliminating panning, which is done for us automatically
-- by virtue of the relative vector spaces of composed subdiagrams.
-- There is already a function called pointAt'.
--pointAt'' :: R3 -> R3 -> R3 -> T3
pointAt'' about initial final = tilt
    where
        tiltAngle = angleBetween initial final
        tiltDir   = direction $ cross3 about final :: Spherical
        tilt      = rotationAbout origin tiltDir tiltAngle

--mkTip :: TreeNode3 -> R3 -> TreeNode3
mkTip (p, v, w) v' = (p .+^ v, v', taperWidth w)

