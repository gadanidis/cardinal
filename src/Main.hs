module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import           Data.List.Split (splitOn)
import           Text.ParserCombinators.ReadP
import           Text.Read (readMaybe)
import           Data.Maybe (fromMaybe)
import           Text.Pretty.Simple (pPrint)

import           System.IO
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let paths = if null args then error "must provide a ballot file as argument"
                             else args
    inputs <- mapM readFile paths
    let ballots = (map (parseBallot . rmSpace) . lines . concat) inputs
    let outcome = result ballots
    let m = maximum $ M.elems outcome
    let winner = M.keys $ M.filter (== m) outcome
    putStrLn $ "The winner is: " ++ head winner ++ "!"
    putStrLn "Vote tallies:"
    (pPrint . M.toAscList) outcome

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

data Result = Result String String
    deriving (Eq, Show)

vote :: ReadP Result
vote = Result <$> anything <* divider <*> anything <* eof

anything :: ReadP String
anything = many1 $ satisfy (/= '|')

divider :: ReadP String
divider = many1 $ satisfy (== '|')

rmSpace :: String -> String
rmSpace "" = ""
rmSpace (c:cs) = if c == ' '
                then rmSpace cs
                else c : rmSpace cs

resultToVote :: Result -> (String, Vote)
resultToVote (Result a b) = fmap toVote (a, b)

parseVote :: String -> Map String Vote
parseVote = M.fromList . fmap resultToVote . map fst . readP_to_S vote

parseBallot :: String -> Map String Vote
parseBallot b = M.unions $ map parseVote (splitOn "," b)

data Vote = Support
          | Oppose
          | Abstain
          deriving (Eq, Show, Read)

toVote :: String -> Vote
toVote x = (fromMaybe (error $ "Unrecognized vote: " ++ x) . readMaybe) x

vote2int :: Vote -> Int
vote2int Support = 1
vote2int Oppose = -1
vote2int Abstain = 0

result :: [Map String Vote] -> Map String Int
result = M.unionsWith (+) . map (M.map vote2int)
