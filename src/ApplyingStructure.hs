module ApplyingStructure where

import           Data.Map    as M
import           Data.Monoid

xs = M.fromList [('a', 1), ('b', 2), ('c', 3)]
ys = M.fromList [('b', 2)]
zs = xs <> ys
