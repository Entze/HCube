module HCube.Data (RubiksCubeFace(Blue, Orange, White, Red, Yellow, Green),
                   RubiksCube(Cube)) where

data RubiksCubeFace = Blue | Orange | White | Red | Yellow | Green deriving (Enum, Show, Eq, Bounded)

data RubiksCube = Cube Int [[[RubiksCubeFace]]] deriving (Eq, Show)
