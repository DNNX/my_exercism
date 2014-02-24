module School (add, empty, sorted, grade, School) where

import qualified Data.Map as M
import           Data.Function (on)
import           Data.List (sortBy, sort)

type Student = String
type Grade   = Int
newtype School = School (M.Map Grade [Student]) deriving (Show, Eq)

add :: Grade -> Student -> School -> School
add g s (School m) = School (M.alter f g m)
                     where f value = case value of 
                                     Just students -> Just (s:students)
                                     Nothing       -> Just [s]

sorted :: School -> [(Grade, [Student])]
sorted (School m) = map (\(g,s) -> (g, sort s)) $ sortBy (compare `on` fst) $ M.assocs m

grade :: Grade -> School -> [Student]
grade g (School m) = sort $ M.findWithDefault [] g m

empty :: School
empty = School M.empty