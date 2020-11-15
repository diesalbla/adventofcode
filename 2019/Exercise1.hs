
module Exercise1 where

{--

Fuel required to launch a given module is based on its mass.
Specifically, to find the fuel required for a module,
take its  mass, divide by three, round down, and subtract 2.
--}
fuelOf x = max 0 $ (x `quot` 3) - 2

{-
The Fuel Counter-Upper needs to know the total fuel requirement.
To find it, individually calculate the fuel needed for the mass
of each module (your puzzle input), then add together all the fuel values.
--}
part1 = sum . map fuelOf

{--
So, for each module mass, calculate its fuel and add it to the total.
Then, treat the fuel amount you just calculated as the input mass and
repeat the process, continuing until a fuel requirement is zero or negative
-}
fuelOfIter  = sum . takeWhile (> 0) . drop 1 . iterate fuelOf
  
-- fuel for all our modules
totalFuel = sum . map fuelOfIter 
