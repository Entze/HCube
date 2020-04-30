module HCube.Util (getCubie, setCubie) where

import HCube.Data (RubiksCubeFace,
                   RubiksCube(Cube))

setAt :: Int -> a -> [a] -> [a]
setAt i e ls = pre ++ (e:post)
  where
    (pre, (_:post)) = splitAt i ls

getCubie :: RubiksCube -> Int -> RubiksCubeFace
getCubie (Cube d fs) n = ((fs !! faceNr) !! rowNr) !! columnNr
  where
    rowNr :: Int
    rowNr = cubieNr `quot` d
    columnNr :: Int
    columnNr = cubieNr `rem` d
    cubieNr :: Int
    cubieNr = n `rem` d2
    faceNr :: Int
    faceNr = n `quot` 6
    d2 :: Int
    d2 = d^2

setCubie :: Int -> RubiksCubeFace -> RubiksCube -> RubiksCube
setCubie n f (Cube d fs) = (Cube d newCube)
  where
    newCube :: [[[RubiksCubeFace]]]
    newCube = setAt faceNr newFace fs
    newFace :: [[RubiksCubeFace]]
    newFace = setAt rowNr newRow face
    newRow :: [RubiksCubeFace]
    newRow = setAt columnNr f row
    face :: [[RubiksCubeFace]]
    face = (fs !! faceNr)
    row :: [RubiksCubeFace]
    row = face !! rowNr
    rowNr :: Int
    rowNr = cubieNr `quot` d
    columnNr :: Int
    columnNr = cubieNr `rem` d
    cubieNr :: Int
    cubieNr = n `rem` d2
    faceNr :: Int
    faceNr = n `quot` d2
    d2 :: Int
    d2 = d^2
