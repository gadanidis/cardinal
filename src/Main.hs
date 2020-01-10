{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import           Data.List.Split (splitOn)
import           Text.ParserCombinators.ReadP
import           Text.Read (readMaybe)
import           Data.Maybe (fromMaybe)
import           Text.Pretty.Simple (pPrint)

import           System.Environment
import           System.Console.Docopt
import           Control.Monad (when)

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    let paths = args `getAllArgs` argument "file"

    inputs <- mapM readFile paths
    let ballots = (map (parseBallot . rmSpace) . lines . concat) inputs
    let outcome = result ballots
    let m = maximum $ M.elems outcome
    let winner = M.keys $ M.filter (== m) outcome
    putStrLn $ "The winner is: " ++ head winner ++ "!"

    when (args `isPresent` longOption "verbose") $ do
        putStrLn "Vote tallies:"
        (pPrint . M.toAscList) outcome

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
parseVote = M.fromList . fmap (resultToVote . fst) . readP_to_S vote

parseBallot :: String -> Map String Vote
parseBallot = M.unions . map parseVote . splitOn ","

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
