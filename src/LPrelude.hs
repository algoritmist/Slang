module LPrelude where

import Language

k' :: b -> c -> c
k' = flip const

sK :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
sK f g x = f x (g x)

compose :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
compose f g x = f (g x)

twice :: (a -> a) -> a -> a
twice f = f . f

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ( "S",
      ["f", "g", "x"],
      EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))
    ),
    ( "compose",
      ["f", "g", "x"],
      EAp (EVar "f") (EAp (EVar "g") (EVar "x"))
    ),
    ( "twice",
      ["f"],
      EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")
    )
  ]
