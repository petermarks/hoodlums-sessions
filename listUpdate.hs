module ListUpdate where

data Operation = Delete | SetTrue

original :: [(Int, Bool)]
original = 
  [ (1000, False)
  , (1001, True)
  , (1004, False)
  , (1006, False)
  ]

updates :: [(Int, Operation)]
updates =
  [ (1003, Delete)
  , (1004, Delete)
  , (1006, SetTrue)]

