-- Copyright (c) 2012 Samuel Rødal
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

import Data.Bits
import Data.List
import qualified Data.Set as Set
import Data.Word
import Prelude hiding (negate)

split :: Char -> String -> [String]
split _ "" = [ "" ]
split y (x : xs)
    | x == y = "" : rest
    | otherwise = (x : rhead) : rtail
        where rest@(rhead : rtail) = split y xs

pieces :: [String]
pieces = [ "neeu", "nene", "nue", "n.w.ne", "n.eu.n", "nn.e.u", "n.e.un", "nneu", "ne.u.e", "n.w.n.e", "nwun", "n.w.u.e", "nwnu" ]

splitPieces = map (split '.') pieces

type Coord = (Int, Int, Int)
type Piece = [Coord]

add (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)
negate (x, y, z) = (-x, -y, -z)
subtractBy x y = add y $ negate x

translateCoord :: Coord -> Char -> Coord
translateCoord c dir  = add c (dx, dy, dz)
    where dx | dir == 'w' = -1 | dir == 'e' = 1 | otherwise = 0
          dy | dir == 'n' = -1 | dir == 's' = 1 | otherwise = 0
          dz | dir == 'u' = 1 | dir == 'd' = -1 | otherwise = 0

parsePiece :: [String] -> Piece
parsePiece (x : xs) = foldl (++) first $ map (tail . offsetPiece . parseElement) xs
    where parseElement = scanl translateCoord (0, 0, 0)
          first = parseElement x
          offsetPiece = map $ add $ last first

maxElement (x, y, z) = max x $ max y z

bound :: (Int -> Int -> Int) -> Piece -> Coord
bound f (x : xs) = bound_helper f x xs
    where bound_helper _ result [] = result
          bound_helper f (mx, my, mz) ((x, y, z)  : xs) = bound_helper f (f x mx, f y my, f z mz) xs

lowerBound = bound min
upperBound = bound max

normalizePiece :: Piece -> Piece
normalizePiece piece = sort $ map (subtractBy $ lowerBound piece) piece

rotateX :: Piece -> Piece
rotateX = normalizePiece . map (\(x, y, z) -> (x, -z, y))

rotateY :: Piece -> Piece
rotateY = normalizePiece . map (\(x, y, z) -> (-z, y, x))

rotateZ :: Piece -> Piece
rotateZ = normalizePiece . map (\(x, y, z) -> (y, -x, z))

allRotations :: Piece -> [Piece]
allRotations piece = Set.toList $ allRotations_helper $ Set.fromList [piece]
    where
        allRotations_helper s
            | (Set.size permuted) == (Set.size s) = s
            | otherwise = allRotations_helper permuted
            where permuted = foldr Set.union s $ map (\f -> Set.map f s) [rotateX, rotateY, rotateZ]

allTranslations :: Piece -> [Piece]
allTranslations piece = filter ((>) 4 . (maxElement . upperBound)) $ map (\offset -> map (add offset) piece) [(dx, dy, dz) | dx <- [0..3], dy <- [0..3], dz <- [0..3]]

parsedPieces = map (normalizePiece . parsePiece) splitPieces

allPermutations = reverse $ map (\(f, p) -> (concat . map allTranslations) $ f p) $ zip ((take 12 $ repeat allRotations) ++ [(\x -> (:) x [])]) parsedPieces

type BitField = Word64

toBit :: Coord -> BitField
toBit (x, y, z) = shiftL 1 (x + (y + z * 4) * 4)

toCoord :: Int -> Coord
toCoord x = (mod x 4, mod (div x 4) 4, div x 16)

toBitField :: Piece -> BitField
toBitField piece = foldr (.|.) 0 $ map toBit piece

fromBitField :: BitField -> Piece
fromBitField field = fromBitField_helper field 0
    where
        fromBitField_helper 0 _ = []
        fromBitField_helper field n
            | (field .&. 1) == 1 = [toCoord n] ++ rest
            | otherwise = rest
                where rest = fromBitField_helper (shiftR field 1) (n + 1)

bitPermutations :: [[BitField]]
bitPermutations = (map $ map toBitField) allPermutations

type Candidate = (Int, BitField)

intersects x y = (x .&. y) /= 0

isLowestBit :: Int -> BitField -> Bool
isLowestBit bit field = (testBit field bit) && (not $ intersects mask field)
    where mask = (shiftL 1 bit) - 1

bitCandidates :: [[Candidate]]
bitCandidates = map (\bit -> filter ((isLowestBit bit) . snd) candidates ) [0..63]
    where candidates = concat . map (\(x, p) -> zip (repeat x) p) $ zip [0..] bitPermutations

nextBit :: BitField -> BitField -> Int
nextBit on x
    | (x .&. 1) /= on = 1 + (nextBit on $ shiftR x 1)
    | otherwise = 0

notUsed mask index = not $ intersects mask $ shift 1 index

solutions :: BitField -> Int -> Int -> Int -> [[Candidate]] -> [Candidate] -> [[Candidate]]
solutions mask currentBit used depth cand pieces
    | depth == 13 = [pieces]
    | otherwise = concat . map solutions_helper $ candidates
        where nextFreeBit = nextBit 0 mask
              delta = nextFreeBit - currentBit
              nextCand = iterate tail cand !! delta
              candidates = filter isCandidate $ head nextCand
              isCandidate (index, field) = (notUsed used index) && (not $ intersects mask field)
              solutions_helper candidate@(index, field) = solutions (mask .|. field) nextFreeBit (used .|. (shift 1 index)) (depth + 1) nextCand (candidate : pieces)

allSolutions = (map $ map (\(index, field) -> (index, fromBitField field))) $ solutions 0 0 0 0 bitCandidates []

type Point = (Double, Double)
type Color = (Int, Int, Int)
type Polygon = (Color, [Point])

toSvgPoints :: [Point] -> String
toSvgPoints = concat . map (\(x,y) -> show x ++ "," ++ show y ++ " ")

toSvgColor (r, g, b) = "rgb(" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"

polygonToSvg p =
  "<polygon points=\"" ++ (toSvgPoints $ snd p) ++ "\" style=\"fill:" ++ (toSvgColor $ fst p) ++ ";stroke:black;stroke-width:1\"/>\n"

generateSvg :: [Polygon] -> String
generateSvg polygons =
    "<?xml version=\"1.0\" standalone=\"no\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400px\" height=\"1120px\" version=\"1.1\">\n" ++
    (concat $ map polygonToSvg polygons) ++ "</svg>"

type Cell = (Color, Coord)

colorForIndex = (!!) [(255, 0, 0), (0, 255, 0), (0, 0, 255), (255, 200, 0), (200, 0, 255), (0, 255, 200), (127, 127, 127), (127, 255, 0), (127, 0, 255), (0, 60, 140), (140, 60, 0), (255, 255, 127), (255, 0, 127)]

pieceToCells :: (Int, Piece) -> [Cell]
pieceToCells (index, piece) = zip (repeat $ colorForIndex index) piece

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1+x2, y1+y2)

scalePoint :: Point -> Point -> Point
scalePoint (x1, y1) (x2, y2) = (x1*x2, y1*y2)

adjustPolygon :: Coord -> [Point] -> [Point]
adjustPolygon (x, y, z) = map (addPoint (100, -50) . scalePoint (10, 10) . addPoint ((fromIntegral $ x-y)*1.25, (fromIntegral $ x+y)*0.3-(fromIntegral z)))

topPolygon coord = adjustPolygon coord $ [(0, 0), (1.25, 0.3), (2.5, 0), (1.25, -0.3)]
leftPolygon coord = adjustPolygon coord $ [(0, 0), (1.25, 0.3), (1.25, 1.3), (0, 1)]
rightPolygon coord = adjustPolygon coord $ [(1.25, 0.3), (2.5, 0), (2.5, 1), (1.25, 1.3)]

topView :: Coord -> [Point]
topView (x, y, _) = map (addPoint (200, -75) . scalePoint (10, 10) . addPoint (fromIntegral x, fromIntegral y)) [ (0, 0), (1, 0), (1, 1), (0, 1) ]

darken :: Color -> Double -> Color
darken (r, g, b) x = (floor $ (fromIntegral r) * x, floor $ (fromIntegral g) * x, floor $ (fromIntegral b) * x)

cellToPolygons :: Point -> (Color, Coord) -> [Polygon]
cellToPolygons offset (color, coord) = zip [color, color, (darken color 0.7), (darken color 0.8)]  $ map (map (addPoint offset)) [ topView coord, topPolygon coord, leftPolygon coord, rightPolygon coord ]

cellOrder (_, (x1, y1, z1)) (_, (x2, y2, z2)) = compare (x1 + y1 + z1) (x2 + y2 + z2)

sortCells cells = sortBy cellOrder cells

subSolutionToPolygons :: ([(Int, Piece)], Point) -> [Polygon]
subSolutionToPolygons (pieces, offset) = concat $ map (cellToPolygons offset) $ sortCells $ (concat $ map pieceToCells pieces)

solutionToPolygons pieces = concat $ map subSolutionToPolygons $ zip (reverse $ tails pieces) [(0, 80 * y) | y <- [0..]]

main = writeFile "out.svg" $ generateSvg $ solutionToPolygons $ head allSolutions
