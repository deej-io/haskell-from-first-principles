module Vehicles where

data Size = Size Integer deriving (Eq, Show)
data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000) :: Vehicle
urCar = Car Mazda (Price 20000) :: Vehicle
clownCar = Car Tata (Price 7000) :: Vehicle
doge = Plane PapuAir (Size 10000000) :: Vehicle

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
-- partial function:(
