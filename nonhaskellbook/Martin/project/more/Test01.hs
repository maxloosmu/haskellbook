-- first attempt to resolve Martin's project

import Data.List
import Data.Maybe
import Data.Char

-- https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell

class01 = "class Vehicle"
class02 = "class Car extends Vehicle"
class03 = "class Truck extends Vehicle"
class04 = "class Day"
class05 = "class Workday extends Day"
class06 = "class Holiday extends Day"
class07 = "class Road"
class08 = "class Highway extends Road"

checkClass :: String -> Bool
checkClass = isPrefixOf "class"
checkExtends :: String -> Maybe Int
checkExtends str =
  findIndex (isPrefixOf "extends") (tails str)
splitClass :: (Num a, Enum a) =>
  String -> [(a, String)]
splitClass str = zip [0..] (words str)

formDecl :: String -> String
formDecl str = "decl is" ++ fromMaybe ""
  (lookup 1 (splitClass str)) ++ " : "
formPred :: String -> String
formPred str = fromMaybe ""
  (lookup 3 (splitClass str))
  ++ " -> Boolean"
classToPred :: String -> String
classToPred str = if checkClass str &&
  isJust (checkExtends str)
  then formDecl str ++ formPred str
  else ""

test01 = classToPred class01
test02 = classToPred class02
test03 = classToPred class03
test04 = classToPred class04
test05 = classToPred class05
test06 = classToPred class06
test07 = classToPred class07
test08 = classToPred class08

