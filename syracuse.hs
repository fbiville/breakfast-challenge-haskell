flyToSyracuse :: Int -> [Int]
flyToSyracuse n = takeWhile (/=1) (infiniteFly n) ++ [1]

highFlyTime :: Int -> Int
highFlyTime n = length $ takeWhile (>=n) $ flyToSyracuse n

maxAltitude n = maximum $ flyToSyracuse n

flightTime n = length $ flyToSyracuse n

infiniteFly :: Int -> [Int]
infiniteFly n = n : infiniteFly (next n)
	where next n = if (even n) then n `div` 2 else 3*n+1

