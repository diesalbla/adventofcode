
module Exercise1 where

-- fuelOf: fuel for one mass
fuelOf = max 0 . (+ (-2)) . (`quot` 3)

-- fuelOfIter: fuel for mass, and for its fuel, and that fuel, etc
fuelOfIter  = sum . takeWhile (> 0) . drop 1 . iterate fuelOf

-- fuel for all our modules
totalFuel = sum . map fuelOfIter 

