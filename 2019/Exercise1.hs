
module Exercise1 where

-- fuelOf: fuel for one mass
fuelOf x = max 0 $ (x `quot` 3) - 2

-- fuelOfIter: fuel for mass, and for its fuel, and that fuel, etc
fuelOfIter  = sum . takeWhile (> 0) . drop 1 . iterate fuelOf

-- fuel for all our modules
totalFuel = sum . map fuelOfIter 
