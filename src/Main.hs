module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

main :: IO ()
main = undefined

ballots :: [Map String Vote]
ballots = map M.fromList [ [("john", Oppose), ("meg", Support)]
                         , [("john", Abstain), ("meg", Support)]
                         , [("john", Support), ("meg", Support)]
                         , [("john", Support), ("meg", Support)]
                         , [("john", Oppose), ("meg", Support)]
                         ]

data Vote = Support
          | Oppose
          | Abstain
          deriving (Eq, Show)

vote2int :: Vote -> Int
vote2int Support = 1
vote2int Oppose = -1
vote2int Abstain = 0

result :: [Map String Vote] -> Map String Int
result = M.unionsWith (+) . map (M.map vote2int)
