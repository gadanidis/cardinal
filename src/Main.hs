module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import           Data.List.Split (splitOn)
import           Text.ParserCombinators.ReadP
import           Data.Char (isLetter)
import           Text.Read (readMaybe)
import           Data.Maybe (fromMaybe)

main :: IO ()
main = do
    putStr "enter input file path: "
    inputFile <- getLine
    input <- readFile inputFile
    let ls = lines (rmSpace input)
    let ballots = map parseBallot ls
    print $ result ballots

vote :: ReadP (String, Vote)
vote = do
    name <- many1 (satisfy isLetter)
    _ <- many1 (satisfy (\x -> x == ':' || x == ' '))
    response <- many1 (satisfy isLetter)
    return (name, toVote response)

rmSpace :: String -> String
-- removes all spaces from a string
rmSpace "" = ""
rmSpace (c:cs) = if c == ' '
                then rmSpace cs
                else c : rmSpace cs

parseVote :: String -> Map String Vote
parseVote = M.fromList . map fst . readP_to_S vote

parseBallot :: String -> Map String Vote
parseBallot b = M.unions $ map parseVote (splitOn "," b)

data Vote = Support
          | Oppose
          | Abstain
          deriving (Eq, Show, Read)

toVote :: String -> Vote
toVote = fromMaybe Abstain . readMaybe

vote2int :: Vote -> Int
vote2int Support = 1
vote2int Oppose = -1
vote2int Abstain = 0

result :: [Map String Vote] -> Map String Int
result = M.unionsWith (+) . map (M.map vote2int)
