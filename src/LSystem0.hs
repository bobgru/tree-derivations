module Main where
import Data.Tree(flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Prelude

main       = defaultMain $ pad 1.1 $ renderTree buildTree
renderTree = mconcat . flatten . fmap drawBranch
buildTree  = unfoldTree growTree seed
seed       = (origin, unitY)
drawBranch (p, v) = position [(p, fromOffsets [v])]

growTree    n = if stop n then branchTip n else branches fs n
branchTip   n = (n, [])
branches fs n = (n, zipWith ($) fs (repeat n))

fs   = [ scaleBranch s . rotateBranch   a  . followBranch,
         scaleBranch s . rotateBranch (-a) . followBranch ]
    where s  = 0.6
          a  = 1/7

stop           (_, v) = magnitude v < 0.05
followBranch   (p, v) = (p .+^ v, v)
scaleBranch  s (p, v) = (p, v ^* s)
rotateBranch t (p, v) = (p, rotateBy t v)
