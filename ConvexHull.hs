-- file: ch03/GrahamScan.hs

import Data.List

data Point = Point (Double,Double)
              deriving (Eq,Show)

data Direction = Direction | Straight
                           | LeftTurn 
                           | RightTurn 
              deriving (Eq,Show)

crossProduct :: Point -> Point -> Point -> Double 
crossProduct (Point (x1,y1)) (Point (x2,y2)) (Point (x3,y3))
             = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

calcTurn :: Point -> Point -> Point -> Direction
calcTurn p1 p2 p3 = case compare (crossProduct p1 p2 p3) 0 of 
                         EQ -> Straight
                         GT -> LeftTurn
                         LT -> RightTurn

getAngle :: Point -> Point -> Double
getAngle (Point (x1,y1)) (Point (x2,y2)) = atan2 (y2 - y1) (x2 - x1)

compareY :: Point -> Point -> Ordering
compareY (Point (x1,y1)) (Point (x2, y2)) | y1 > y2   = GT
                                          | y1 < y2   = LT
                                          | x1 > x2   = GT
                                          | x1 < x2   = LT

compareAngle :: Point -> Point -> Point -> Ordering
compareAngle pvt p1 p2 | getAngle pvt p1 > getAngle pvt p2 = GT
                       | getAngle pvt p1 < getAngle pvt p2 = LT
                       | otherwise                         = EQ 

removeTail :: [a] -> [a]
removeTail xs = reverse (drop 1 (reverse xs))

lowestY :: [Point] -> Point
lowestY ps = minimumBy compareY ps

sortPoints :: [Point] -> [Point]
sortPoints ps = sortBy (compareAngle (lowestY ps)) ps

convexHull :: [Point] -> [Point]
convexHull ps = nub (grahamScan [] (nub (sortPoints ps)))

grahamScan :: [Point] -> [Point] -> [Point]
grahamScan lst [] = []
grahamScan lst (p1:[]) = []
grahamScan lst (p1:p2:[]) = []

grahamScan lst (p1:p2:p3:ps) | ps == [] && lst == [] = case (calcTurn p1 p2 p3) == RightTurn of 
                                                       True  -> lst ++ [p1] ++ [p3]
                                                       False -> lst ++ [p1] ++ [p2] ++ [p3]

                             | lst == [] = case (calcTurn p1 p2 p3) == RightTurn of
                                           True   -> grahamScan [] (p1:p3:ps)
                                           False  -> grahamScan [p1] (p2:p3:ps)

                             | otherwise = case (calcTurn p1 p2 p3) == RightTurn of
                                           True  -> grahamScan (removeTail lst) (head (reverse lst):p1:p3:ps)
                                           False -> grahamScan (lst ++ [p1]) (p2:p3:ps)

ctest  = [Point(-3,7),Point(-2,6),Point(-1,4),Point(0,1),Point(0,0),Point(1,4),Point(2,6),Point(3,7)]
ctest2 = [Point(0,0),Point(1,1),Point(-1,1),Point(0.5,0.9),Point(0,0.7)]
