module Utils where

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf = map fst

rhssOf :: [(a, b)] -> [b]
rhssOf = map snd
