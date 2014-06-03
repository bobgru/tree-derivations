import Data.Tree(flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Prelude

main       = defaultMain $ pad 1.1 $ renderTree $ buildTree
renderTree = mconcat . flatten . fmap drawBranch
buildTree  = unfoldTree branches seed
seed       = (origin, unitY)
drawBranch (p, v) = position [(p, fromOffsets [v])]
branches   (p, v)
    | magnitude v < 0.05  =  ((p, v), [])
    | otherwise           =  ((p, v), pvs)
    where pvs  = [(p', br (1/7)), (p', br (-1/7))]
          p'   = p .+^ v
          br a = v # scale 0.6 # rotateBy a
