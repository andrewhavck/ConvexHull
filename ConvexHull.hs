-- file: ch03/GrahamScan.hs

import Data.List

data Point = Point (Double,Double)
              deriving (Show)

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

cotangent :: Point -> Point -> Double
cotangent (Point (x1,y1)) (Point (x2,y2)) = (x2 - x1) / (y2 - y1)

compareY :: Point -> Point -> Ordering
compareY (Point (x1,y1)) (Point (x2, y2)) | y1 > y2   = GT
                                          | y1 < y2   = LT
                                          | x1 > x2   = GT
                                          | x1 < x2   = LT

compareAngle :: Point -> Point -> Point -> Ordering
compareAngle pvt p1 p2 | cotangent pvt p1 > cotangent pvt p2 = GT
                       | cotangent pvt p1 < cotangent pvt p2 = LT
                       | otherwise                           = EQ 

lowestY :: [Point] -> Point
lowestY ps = minimumBy compareY ps

sortPoints :: [Point] -> [Point]
sortPoints ps = sortBy (compareAngle (lowestY ps)) ps

convexHull :: [Point] -> [Point]
convexHull ps = grahamScan (sortPoints ps)

grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan (p1:[]) = []
grahamScan (p1:p2:[]) = []
grahamScan (p1:p2:p3:[]) = case (calcTurn p1 p2 p3) == RightTurn of
                             False -> [p1] ++ [p2] ++ [p3]
                             True  -> []

grahamScan (p1:p2:p3:p4:ps) = case (calcTurn p1 p2 p3) == RightTurn of
                                   True   -> grahamScan(p1:p3:p4:ps)
                                   False  -> [p1] ++ [p2] ++ [p3] ++ grahamScan(p4:ps)
