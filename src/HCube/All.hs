module HCube.All (RubiksCubeFace(Blue, Orange, White, Red, Yellow, Green),
                  RubiksCube(Cube),
                  isCanonical,
                  solved,
                  getCubie,
                  setCubie) where

import HCube.Data (RubiksCubeFace(Blue, Orange, White, Red, Yellow, Green),
                   RubiksCube(Cube))
import HCube.Util


{--
instance Show RubiksCube where
  show (Cube d fs) = showFaces 0 d fs
    where
      showFaces n d fs = setupFace n d ++ showFaces
--}

isCanonical :: RubiksCube -> Bool
isCanonical (Cube d fs) = length fs == 6 && all ((== d) . length) fs && all (all ((== d) . length)) fs

solved :: Int -> RubiksCube
solved 0 = Cube 0 []
solved n
  | n <= 0 = error "Rubiks Cubes with negative dimensions do not exist"
  | otherwise = Cube n (map ((replicate n) . (replicate n)) [(minBound :: RubiksCubeFace)..])
