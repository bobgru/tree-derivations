module Main where
import Data.Tree(Tree, flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
--import Diagrams.Backend.SVG(SVG)
import Diagrams.Prelude
import Diagrams.ThreeD.Transform(aboutY)
import Diagrams.ThreeD.Types(unp3)
import Diagrams.ThreeD.Vector(unitZ)

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
    tcCutOff       = 0.05 * s,
    tcMinLineWidth = 0.01 * s,
    tcInitialWidth = 0.1  * s,
    tcWidthTaper   = 0.7,
    tcBranchScale  = 0.6,
    tcBranchAngle  = 1/7
}
    where s = 10000

--main :: IO ()
main = defaultMain $ pad 1.1 $ renderTree buildTree

--renderTree :: Tree TreeNode3 -> Dgm
renderTree = mconcat . flatten . fmap drawBranch . fmap projectNode

--buildTree :: Tree TreeNode3
buildTree = unfoldTree branches seed

--seed :: TreeNode3
seed = (origin, unitZ ^* tcScale tc, tcInitialWidth tc)

--projectNode :: TreeNode3 -> TreeNode
projectNode (p, v, w) = (p', v', w)
    where q  = p .+^ v
          q' = projectPtXZ q
          p' = projectPtXZ p
          v' = q' .-. p'

--projectPtXZ :: P3 -> P2
projectPtXZ p = case unp3 p of (x, _, z) -> p2 (x, z)

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
          p'    = p .+^ v
          w'    = taperWidth w
          n     = v # rotateBy (1/4) # normalized
          w2    = w  / 2 ; w2' = w' / 2
          a     = p  .-^ (w2  *^ n) ; b = p' .-^ (w2' *^ n)
          c     = p' .+^ (w2' *^ n) ; d = p  .+^ (w2  *^ n)

--taperWidth :: Double -> Double
taperWidth w = max (w * tcWidthTaper tc) (tcMinLineWidth tc)

--branches :: TreeNode3 -> (TreeNode3, [TreeNode3])
branches n@(_, v, _)
    | magnitude v < tcCutOff tc  =  (n, [])
    | otherwise                  =  (n, branchTips n)

--branchTips :: TreeNode3 -> [TreeNode3]
branchTips (p, v, w) = [(p', br a, w'), (p', br (-a), w')]
    where p'   = p .+^ v
          br a = v # scale (tcBranchScale tc) # t a
          a    = tcBranchAngle tc
          w'   = taperWidth w
          t a  = transform (aboutY (a @@ turn))
