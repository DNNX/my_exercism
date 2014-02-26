module School (add, empty, sorted, grade, School) where

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import           Data.Function (on)
import           Data.List (sortBy)

type Student = String
type Grade   = Int
type School = M.HashMap Grade (S.Set Student)

add :: Grade -> Student -> School -> School
add grade student school = M.insertWith S.union grade (S.singleton student) school

sorted :: School -> [(Grade, [Student])]
sorted = sortBy (compare `on` fst) . M.toList . M.map S.toList

grade :: Grade -> School -> [Student]
grade g school = S.toList $ M.lookupDefault S.empty g school

empty :: School
empty = M.empty